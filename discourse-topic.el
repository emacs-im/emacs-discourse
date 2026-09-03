;;; discourse-topic.el --- Discourse topic post streams -*- lexical-binding: t; -*-

;;; Commentary:

;; Stream-authoritative topic pagination projected through Appkit discussion
;; rows.  Cooked post HTML is adapted to Appkit semantic markup before native
;; insertion.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-chat-ins)
(require 'appkit-invalidation)
(require 'appkit-scroll)
(require 'appkit-markup)
(require 'appkit-markup-ui)
(require 'appkit-media-card)
(require 'appkit-position)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'appkit-view)
(require 'discourse-api)
(require 'discourse-compose)
(require 'discourse-customize)
(require 'discourse-markup)
(require 'discourse-media)
(require 'discourse-runtime)
(require 'discourse-state)
(require 'discourse-site)
(require 'discourse-ui)
(declare-function discourse-topic-transient "discourse-transient" ())
(declare-function discourse-topic-list-open-latest
                  "discourse-topic-list" (account &optional select))


(defconst discourse-topic--request-key 'posts
  "View request-table key for the active topic request.")

(defvar-local discourse-topic--scroll-observer nil
  "Lifecycle-owned automatic pagination observer for this topic.")

(defconst discourse-topic-post-id-property 'discourse-post-id
  "Text property carrying a stable Discourse post ID.")

(defconst discourse-topic-post-number-property 'discourse-post-number
  "Text property carrying a topic-local post number.")

(cl-defstruct (discourse-topic-state
               (:constructor discourse-topic-state-create)
               (:copier nil))
  account
  topic-id
  stream
  loaded-ids
  phase
  message
  request-token
  loaded-p
  exhausted-p
  target-post-number
  back-stack
  retry-phase
  retry-post-ids)

(defun discourse-topic--state (&optional view)
  "Return validated topic state for VIEW or the current view."
  (let* ((view (or view (appkit-current-view)))
         (state (and (appkit-view-live-p view) (appkit-view-state view))))
    (unless (and (discourse-topic-state-p state)
                 (discourse-account-p (discourse-topic-state-account state)))
      (error "Current view has no Discourse topic state"))
    state))

(defun discourse-topic--field (object key &optional default)
  "Return OBJECT string KEY or DEFAULT."
  (if (hash-table-p object) (gethash key object default) default))

(defun discourse-topic--string (value &optional fallback)
  "Return property-free string VALUE or FALLBACK."
  (if (stringp value) (substring-no-properties value) (or fallback "")))

(defun discourse-topic--nonempty-string (value)
  "Return property-free VALUE when it is a non-empty string."
  (and (stringp value)
       (not (string-empty-p value))
       (substring-no-properties value)))

(defun discourse-topic--sender-name (post)
  "Return POST's visible sender identity."
  (or (discourse-topic--nonempty-string
       (discourse-topic--field post "display_username"))
      (discourse-topic--nonempty-string
       (discourse-topic--field post "username"))
      "(deleted user)"))

(defun discourse-topic--sender-id (post)
  "Return POST's sender ID as an opaque decimal string, or nil."
  (condition-case nil
      (discourse-state-id (discourse-topic--field post "user_id"))
    (error nil)))

(defun discourse-topic--observe-post-author (state post)
  "Merge POST's embedded author observation into canonical STATE."
  (when-let* ((user-id (discourse-topic--sender-id post))
              (username
               (discourse-topic--nonempty-string
                (discourse-topic--field post "username"))))
    (let ((user (make-hash-table :test #'equal)))
      (puthash "id" user-id user)
      (puthash "username" username user)
      (dolist (key '("name" "avatar_template"))
        (when-let* ((value (discourse-topic--field post key)))
          (puthash key value user)))
      (discourse-state-merge-user state user))))

(defun discourse-topic--avatar-fallback (post)
  "Return a compact textual avatar fallback for POST."
  (let ((username (discourse-topic--sender-name post)))
    (if (string-empty-p username)
        "@"
      (upcase (substring username 0 1)))))

(defun discourse-topic--post-id (post)
  "Return POST's stable ID."
  (discourse-state-id (discourse-topic--field post "id")))

(defun discourse-topic--canonical-topic (state)
  "Return canonical topic object for topic STATE."
  (discourse-state-topic
   (discourse-account-state (discourse-topic-state-account state))
   (discourse-topic-state-topic-id state)))

(defun discourse-topic--canonical-post (state post-id)
  "Return canonical POST-ID object for topic STATE."
  (discourse-state-post
   (discourse-account-state (discourse-topic-state-account state)) post-id))

(defun discourse-topic--ordered-posts (state)
  "Return loaded posts in STATE's authoritative stream order."
  (let (posts)
    (dolist (post-id (discourse-topic-state-stream state) (nreverse posts))
      (when (gethash post-id (discourse-topic-state-loaded-ids state))
        (when-let* ((post (discourse-topic--canonical-post state post-id)))
          (push post posts))))))

(defun discourse-topic--format-time (post)
  "Return compact creation time for POST."
  (let ((value (discourse-topic--field post "created_at")))
    (if (not (stringp value))
        ""
      (condition-case nil
          (format-time-string "%Y-%m-%d %H:%M" (date-to-time value))
        (error value)))))

(defun discourse-topic--like-count (post)
  "Return POST's like count from `actions_summary'."
  (let ((actions
         (condition-case nil
             (discourse-state-sequence-list
              (discourse-topic--field post "actions_summary"))
           (error nil))))
    (or
     (cl-loop for action in actions
              when (and (hash-table-p action)
                        (= 2 (or (gethash "id" action) -1)))
              return (let ((count (gethash "count" action)))
                       (and (integerp count) count)))
     0)))

(defun discourse-topic--post-footer (post)
  "Return compact metadata footer for POST."
  (let ((likes (discourse-topic--like-count post))
        (replies (discourse-topic--field post "reply_count" 0))
        (reads (discourse-topic--field post "reads" 0)))
    (string-join
     (delq nil
           (list
            (and (> likes 0) (format "%d like%s" likes
                                     (if (= likes 1) "" "s")))
            (and (integerp replies) (> replies 0)
                 (format "%d repl%s" replies
                         (if (= replies 1) "y" "ies")))
            (and (integerp reads) (> reads 0) (format "%d reads" reads))
            (and (eq t (discourse-topic--field post "accepted_answer"))
                 "accepted answer")))
     " · ")))

(defun discourse-topic--insert-post-context (view post)
  "Insert POST number and an actionable reply reference for VIEW."
  (let ((number (discourse-topic--field post "post_number"))
        (reply-to (discourse-topic--field post "reply_to_post_number")))
    (insert (if (integerp number) (format "#%d" number) "post"))
    (when (integerp reply-to)
      (insert " · replying to ")
      (let ((start (point)))
        (insert (format "#%d" reply-to))
        (appkit-ui-add-action
         start (point)
         (lambda ()
           (with-current-buffer (appkit-view-buffer view)
             (discourse-topic-jump-to-post-number reply-to)))
         :face 'link
         :help-echo (format "Jump to post #%d" reply-to))))))

(defun discourse-topic--markup-fallback-document (node)
  "Return NODE's fallback as an Appkit document."
  (cond
   ((appkit-markup-object-p node)
    (appkit-markup-document
     (list
      (appkit-markup-paragraph
       (appkit-markup-object-fallback node)))))
   ((appkit-markup-object-block-p node)
    (appkit-markup-document
     (appkit-markup-object-block-fallback node)))
   (t (appkit-markup-document nil))))

(defun discourse-topic--markup-value (node)
  "Return validated Discourse provider value from NODE."
  (let ((value
         (cond
          ((appkit-markup-object-p node)
           (appkit-markup-object-value node))
          ((appkit-markup-object-block-p node)
           (appkit-markup-object-block-value node)))))
    (and (discourse-markup-provider-object-p value) value)))

(defun discourse-topic--insert-fallback (node &optional face action)
  "Insert NODE fallback with optional FACE and ACTION."
  (let ((start (point)))
    (appkit-markup-ui-insert-document
     (discourse-topic--markup-fallback-document node)
     :final-newline-p (appkit-markup-object-block-p node)
     :interactive-p nil)
    (when face
      (add-face-text-property start (point) face 'append))
    (when (and action (< start (point)))
      (appkit-ui-add-action start (point) action :face face))))

(defun discourse-topic--media-title (data)
  "Return a useful image-card title from provider DATA."
  (or (discourse-topic--nonempty-string (plist-get data :name))
      (discourse-topic--nonempty-string (plist-get data :alt))
      (when-let* ((url
                   (discourse-topic--nonempty-string
                    (plist-get data :url))))
        (file-name-nondirectory
         (car (split-string url "[?#]"))))
      "image"))

(defun discourse-topic--media-dimensions (data)
  "Return DATA dimensions as compact text, or nil."
  (let ((width (plist-get data :width))
        (height (plist-get data :height)))
    (when (and width height)
      (format "%s×%s" width height))))

(defun discourse-topic--insert-image-card (node value data)
  "Insert NODE as an Appkit image card using provider VALUE and DATA."
  (let* ((view (appkit-current-view))
         (state (discourse-topic--state view))
         (account (discourse-topic-state-account state))
         (context-data (plist-get data :context))
         (post-id (and (listp context-data)
                       (plist-get context-data :post-id)))
         (url (discourse-topic--nonempty-string
               (plist-get data :url)))
         (preview-url
          (or (discourse-topic--nonempty-string
               (plist-get data :preview-url))
              url))
         (title (discourse-topic--media-title data))
         (information
          (discourse-topic--nonempty-string
           (plist-get data :information)))
         (preview
          (and preview-url
               (discourse-media-image-preview
                account preview-url post-id)))
         (status
          (and preview-url
               (discourse-media-image-preview-status
                account preview-url)))
         (open-action
          (and url
               (lambda ()
                 (discourse-media-open-image
                  account url :owner view))))
         (card-context
          (appkit-media-card-context-create
           :payload value
           :kind 'photo
           :title title
           :open-action open-action
           :copy-url-action
           (and url
                (lambda ()
                  (discourse-media-copy-url url))))))
    (appkit-chat-ins-insert-media-card
     :kind 'photo
     :title title
     :details (delq nil
                    (list (discourse-topic--media-dimensions data)))
     :meta information
     :border-face 'font-lock-comment-face
     :title-face 'bold
     :meta-face 'shadow
     :context card-context
     :open-action open-action
     :open-help-echo "Open image in Emacs"
     :body-inserter
     (lambda (prefix-state)
       (let ((start (point)))
         (if preview
             (appkit-media-insert-image-slices
              preview open-action nil
              (format "[Image: %s]" title)
              "Open image in Emacs")
           (insert
            (pcase status
              ('disabled "[preview disabled]")
              ('missing "[image unavailable]")
              (_ "[loading preview]"))))
         (insert "\n")
         (appkit-ui-apply-line-prefix
          start (point) prefix-state))))
    ;; A malformed provider should still preserve its semantic fallback.
    (when (and (null url) (null preview-url))
      (discourse-topic--insert-fallback node 'shadow))))

(defun discourse-topic--onebox-provider (data)
  "Return a compact provider label for onebox DATA."
  (or (discourse-topic--nonempty-string (plist-get data :provider))
      (when-let* ((url
                   (discourse-topic--nonempty-string
                    (plist-get data :url)))
                  (parsed (ignore-errors
                            (url-generic-parse-url url)))
                  (host (url-host parsed)))
        host)
      "link"))

(defun discourse-topic--insert-onebox-card (value data)
  "Insert provider VALUE and DATA as an Appkit rich embed card."
  (let* ((view (appkit-current-view))
         (state (discourse-topic--state view))
         (account (discourse-topic-state-account state))
         (context-data (plist-get data :context))
         (post-id (and (listp context-data)
                       (plist-get context-data :post-id)))
         (url (discourse-topic--nonempty-string
               (plist-get data :url)))
         (image-url
          (discourse-topic--nonempty-string
           (plist-get data :image-url)))
         (provider (discourse-topic--onebox-provider data))
         (title
          (or (discourse-topic--nonempty-string
               (plist-get data :title))
              url
              "Embedded link"))
         (description
          (discourse-topic--nonempty-string
           (plist-get data :description)))
         (preview
          (and image-url
               (discourse-media-image-preview
                account image-url post-id)))
         (preview-status
          (and image-url
               (discourse-media-image-preview-status
                account image-url)))
         (open-action (and url (lambda () (browse-url url))))
         (context
          (appkit-media-card-context-create
           :payload value
           :kind 'embed
           :title title
           :open-action open-action
           :copy-url-action
           (and url
                (lambda ()
                  (discourse-media-copy-url url)))))
         (prefix-state
          (appkit-ui-card-prefix-state
           :face 'font-lock-comment-face))
         (start (point)))
    (appkit-chat-ins-insert-prefixed-line
     provider :prefix prefix-state :face 'shadow)
    (appkit-chat-ins-insert-prefixed-line
     title
     :prefix prefix-state
     :face 'bold
     :action open-action
     :help-echo (and url (format "Open %s" url)))
    (when description
      (appkit-chat-ins-insert-prefixed-line
       description :prefix prefix-state))
    (when image-url
      (let ((preview-start (point)))
        (if preview
            (appkit-media-insert-image-slices
             preview open-action nil "[embed preview]"
             (and url (format "Open %s" url)))
          (insert
           (pcase preview-status
             ('disabled "[preview disabled]")
             ('missing "[preview unavailable]")
             (_ "[loading preview]"))))
        (insert "\n")
        (appkit-ui-apply-line-prefix
         preview-start (point) prefix-state)))
    (add-text-properties
     start (point)
     (list appkit-media-card-context-property context))))

(defun discourse-topic--insert-markup-object (node)
  "Insert one Discourse provider object NODE natively."
  (let* ((value (discourse-topic--markup-value node))
         (kind (and value (discourse-markup-provider-object-kind value)))
         (data (and value (discourse-markup-provider-object-data value)))
         (url (and (listp data) (plist-get data :url))))
    (pcase kind
      ((or 'image 'media)
       (discourse-topic--insert-image-card node value data))
      ('onebox
       (discourse-topic--insert-onebox-card value data))
      ('lazy-video
       (discourse-topic--insert-fallback
        node 'link
        (and (stringp url) (lambda () (browse-url url)))))
      ((or 'mention 'group-mention 'footnote-reference 'footnote-backref)
       (discourse-topic--insert-fallback node 'font-lock-variable-name-face))
      ((or 'math 'asciimath)
       (discourse-topic--insert-fallback node 'appkit-markup-code-face))
      ('checklist
       (discourse-topic--insert-fallback
        node (if (plist-get data :checked-p) 'success 'shadow)))
      ((or 'spoiler 'details)
       ;; The first anonymous slice exposes content with an explicit visual
       ;; boundary; view-local collapse/reveal state can replace this without
       ;; changing the semantic adapter.
       (discourse-topic--insert-fallback node 'shadow))
      (_ (discourse-topic--insert-fallback node)))))

(defun discourse-topic--same-origin-path (account url)
  "Return URL path when URL belongs to ACCOUNT's exact origin."
  (let ((origin (discourse-account-origin account)))
    (when (and (stringp url)
               (string-prefix-p (concat origin "/") url))
      (substring url (length origin)))))

(defun discourse-topic--link-action (account url)
  "Return native action for validated URL under ACCOUNT."
  (let ((path (discourse-topic--same-origin-path account url)))
    (cond
     ((and path
           (string-match
            "\\`/t/\\(?:[^/?#]+/\\)?\\([1-9][0-9]*\\)\\(?:/\\([1-9][0-9]*\\)\\)?"
            path))
      (let ((topic-id (match-string 1 path))
            (post-number
             (and (match-string 2 path)
                  (string-to-number (match-string 2 path)))))
        (lambda ()
          (discourse-topic-open account topic-id t post-number))))
     ((stringp url) (lambda () (browse-url url))))))

(defun discourse-topic--insert-body (view post prefix properties)
  "Insert POST body in VIEW with PREFIX and outer PROPERTIES."
  (let* ((state (discourse-topic--state view))
         (account (discourse-topic-state-account state))
         (cooked
          (discourse-topic--string
           (discourse-topic--field post "cooked")))
         (document
          (discourse-markup-parse
           cooked
           (discourse-account-origin account)
           :context
           (list :topic-id (discourse-topic-state-topic-id state)
                 :post-id (discourse-topic--post-id post)))))
    ;; Markup applies the discussion body prefix after insertion.  Cards
    ;; therefore need only their border marker here; Appkit's ordinary
    ;; four-column standalone card indent would otherwise be nested a second
    ;; time under the post prefix.
    (let ((appkit-ui-card-indent-prefix ""))
      (appkit-markup-ui-insert-document
       document
       :prefix prefix
       :properties properties
       :final-newline-p t
       :interactive-p t
       :link-action
       (lambda (url) (discourse-topic--link-action account url))
       :object-inserter #'discourse-topic--insert-markup-object))))

(defun discourse-topic--entry (view post)
  "Return Appkit discussion entry for POST in VIEW."
  (let* ((state (discourse-topic--state view))
         (account (discourse-topic-state-account state))
         (id (discourse-topic--post-id post))
         (user-id (discourse-topic--sender-id post))
         (number (discourse-topic--field post "post_number"))
         (properties
          (list discourse-topic-post-id-property id
                discourse-topic-post-number-property number
                'discourse-post post)))
    (appkit-discussion-entry-create
     :key id
     :avatar (and user-id
                  (discourse-media-avatar-image account user-id))
     :avatar-fallback (discourse-topic--avatar-fallback post)
     :context-inserter
     (lambda () (discourse-topic--insert-post-context view post))
     :context-face 'shadow
     :heading (discourse-topic--sender-name post)
     :heading-face 'bold
     :time (discourse-topic--format-time post)
     :body-inserter
     (lambda (prefix row-properties)
       (discourse-topic--insert-body view post prefix row-properties))
     :footer (discourse-topic--post-footer post)
     :properties properties)))

(defun discourse-topic--print-row (row)
  "Insert projected post ROW through Appkit discussion geometry."
  (let ((view (appkit-current-view)))
    (unless (appkit-view-live-p view)
      (error "No live Discourse topic view while rendering a post"))
    (appkit-discussion-insert-entry
     (discourse-topic--entry view (appkit-projection-row-payload row))
     :width (or (appkit-view-window-fill-column) 80))))

(defun discourse-topic--project (state)
  "Project loaded posts from topic STATE."
  (appkit-projection-project
   (discourse-topic--ordered-posts state)
   #'discourse-topic--post-id
   :dependencies-function
   (lambda (post)
     (delq nil
           (list (list :post (discourse-topic--post-id post))
                 (when-let* ((user-id
                              (discourse-topic--sender-id post)))
                   (list :user user-id)))))))

(defun discourse-topic--domain-state (state)
  "Return account-owned canonical state for topic STATE."
  (discourse-account-state (discourse-topic-state-account state)))

(defun discourse-topic--site-title (state)
  "Return the best available site title for topic STATE."
  (let* ((profile
          (discourse-state-site-profile
           (discourse-topic--domain-state state)))
         (title (and profile (gethash "title" profile))))
    (discourse-topic--string
     title
     (discourse-account-origin
      (discourse-topic-state-account state)))))

(defun discourse-topic--header (state)
  "Return generated content header for topic STATE."
  (when-let* ((topic (discourse-topic--canonical-topic state)))
    (let ((title
           (discourse-topic--string
            (discourse-topic--field topic "title")
            (format "Topic %s" (discourse-topic-state-topic-id state))))
          (tags (discourse-ui-topic-tags topic))
          (category
           (discourse-ui-topic-category-text
            topic (discourse-topic--domain-state state))))
      (concat
       (discourse-ui-topic-status-text topic)
       (propertize title 'face '(:height 1.2 :weight bold))
       "\n"
       category
       (unless (string-empty-p tags) (concat "  ·  " tags))
       "\n\n"))))

(defun discourse-topic--header-line ()
  "Return the persistent header line for the current topic view."
  (condition-case nil
      (let* ((state (discourse-topic--state))
             (loaded (hash-table-count
                      (discourse-topic-state-loaded-ids state)))
             (total (length (discourse-topic-state-stream state)))
             (phase
              (pcase (discourse-topic-state-phase state)
                ('initial "loading")
                ('refresh "refreshing")
                ('posts "loading posts")
                ('error "error · R retry")
                (_ (format "%d/%d posts" loaded total)))))
        (concat
         " "
         (propertize (discourse-topic--site-title state) 'face 'bold)
         (propertize
          (format " · t/%s · %s · %s · b Latest · ? actions "
                  (discourse-topic-state-topic-id state)
                  (discourse-account-display-identity
                   (discourse-topic-state-account state))
                  phase)
          'face 'shadow)))
    (error " Discourse · Topic ")))

(defun discourse-topic--footer (state)
  "Return generated footer for topic STATE."
  (let ((loaded (hash-table-count
                 (discourse-topic-state-loaded-ids state)))
        (total (length (discourse-topic-state-stream state))))
    (concat
     "\n"
     (pcase (discourse-topic-state-phase state)
       ('initial "Loading topic…")
       ('refresh (format "Refreshing topic… · %d/%d retained" loaded total))
       ('posts (format "Loading more posts… · %d/%d loaded" loaded total))
       ('error
        (propertize
         (format "%s · R retry · %s"
                 (if (eq (discourse-topic-state-retry-phase state) 'posts)
                     "Unable to load more posts"
                   "Unable to load topic")
                 (or (discourse-topic-state-message state) "unknown error"))
         'face 'error))
       (_
        (propertize
         (format "%d/%d posts loaded%s"
                 loaded total
                 (if (discourse-topic-state-exhausted-p state)
                     " · complete"
                   " · scroll for more"))
         'face 'shadow)))
     "\n")))

(defun discourse-topic--position-intent (events)
  "Return effective semantic position intent from EVENTS.
An explicit post key wins over the default initial `first' position."
  (or (cl-loop for event in (reverse events)
               for position = (plist-get event :position)
               when (and position
                         (not (memq position '(first preserve))))
               return position)
      (and (seq-some
            (lambda (event)
              (eq (plist-get event :position) 'first))
            events)
           'first)
      (cl-loop for event in (reverse events)
               for position = (plist-get event :position)
               when position return position)
      'preserve))

(defun discourse-topic--sync (view invalidations)
  "Synchronize topic VIEW from coalesced INVALIDATIONS."
  (let* ((state (discourse-topic--state view))
         (events (appkit-view-pending-events-snapshot view))
         (event-count (length events))
         (parts (appkit-invalidations-parts invalidations))
         (entry-keys (appkit-invalidations-entry-keys invalidations))
         (resources (appkit-invalidations-resource-keys invalidations))
         (geometry-p (memq 'geometry parts))
         (reconcile-p
          (or geometry-p
              (appkit-invalidations-structure-p invalidations)
              (memq 'entries parts)
              entry-keys resources))
         (rows (and reconcile-p (discourse-topic--project state))))
    (appkit-projection-sync
     view rows
     :header (discourse-topic--header state)
     :footer (discourse-topic--footer state)
     :force-keys
     (if geometry-p
         (mapcar #'appkit-projection-row-key rows)
       entry-keys)
     :changed-dependencies resources
     :position (discourse-topic--position-intent events)
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)
    (force-mode-line-update t)
    (when (and (discourse-topic-state-loaded-p state)
               (appkit-scroll-observer-p discourse-topic--scroll-observer))
      (appkit-scroll-observer-check discourse-topic--scroll-observer))))

(defun discourse-topic--request-current-p (view state token)
  "Return non-nil when TOKEN may update STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq token (discourse-topic-state-request-token state))))

(defun discourse-topic--retire-request (view state token)
  "Retire VIEW request-table entry owned by TOKEN."
  (when (discourse-topic--request-current-p view state token)
    (remhash discourse-topic--request-key
             (appkit-view-request-table view))))

(defun discourse-topic--failure-message (result)
  "Return presentation message for failed RESULT."
  (let ((failure (discourse-http-result-failure result)))
    (if (discourse-http-failure-p failure)
        (discourse-http-failure-message failure)
      "Unknown Discourse response failure")))

(defun discourse-topic--handle-error
    (view state token phase post-ids result)
  "Install failed RESULT and retry identity for TOKEN in VIEW STATE."
  (when (discourse-topic--request-current-p view state token)
    (setf (discourse-topic-state-phase state) 'error
          (discourse-topic-state-message state)
          (discourse-topic--failure-message result)
          (discourse-topic-state-request-token state) nil
          (discourse-topic-state-retry-phase state) phase
          (discourse-topic-state-retry-post-ids state)
          (and post-ids (copy-sequence post-ids)))
    (appkit-request-sync view :part 'frame :position t)
    (unless (eq phase 'posts)
      (message "%s" (discourse-topic-state-message state)))))

(defun discourse-topic--retain-loaded (state stream)
  "Return loaded-ID table from STATE intersected with STREAM."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (post-id stream table)
      (when (and (gethash post-id (discourse-topic-state-loaded-ids state))
                 (discourse-topic--canonical-post state post-id))
        (puthash post-id t table)))))

(defun discourse-topic--update-buffer-name (view state)
  "Update VIEW buffer name from canonical topic STATE."
  (when-let* ((topic (discourse-topic--canonical-topic state))
              (title (discourse-topic--field topic "title"))
              ((stringp title)))
    (with-current-buffer (appkit-view-buffer view)
      (rename-buffer
       (format "*Discourse: %s · t/%s %s*"
               (discourse-topic--site-title state)
               (discourse-topic-state-topic-id state)
               (truncate-string-to-width title 48 nil nil "…"))
       t))))

(defun discourse-topic--request-site-metadata (view)
  "Ensure shared public site metadata needed by VIEW."
  (let* ((state (discourse-topic--state view))
         (account (discourse-topic-state-account state)))
    (discourse-site-ensure-metadata
     account
     (lambda (_kind)
       (when (and (appkit-view-live-p view)
                  (eq state (appkit-view-state view)))
         (discourse-topic--update-buffer-name view state)
         (appkit-request-sync view :part 'frame)))
     :owner view)))

(defun discourse-topic--post-id-for-number (state post-number)
  "Return loaded post ID in STATE for POST-NUMBER."
  (cl-loop for post in (discourse-topic--ordered-posts state)
           when (= post-number
                   (or (discourse-topic--field post "post_number") -1))
           return (discourse-topic--post-id post)))

(defun discourse-topic--focus-post-id (view post-id)
  "Move VIEW to stable POST-ID."
  (appkit-view-enqueue-event view (list :position post-id))
  (appkit-request-sync view :part 'entries :position t))

(defun discourse-topic-jump-to-post-number (post-number)
  "Jump to POST-NUMBER, loading its stream page when necessary."
  (interactive "nPost number: ")
  (unless (and (integerp post-number) (> post-number 0))
    (user-error "Post number must be positive"))
  (let* ((view (appkit-current-view))
         (state (discourse-topic--state view))
         (source-id
          (or (get-text-property (point) discourse-topic-post-id-property)
              (and (> (point) (point-min))
                   (get-text-property
                    (1- (point)) discourse-topic-post-id-property))))
         (target-id
          (discourse-topic--post-id-for-number state post-number)))
    (unless (equal source-id target-id)
      (when (and source-id
                 (not (equal
                       source-id
                       (car (discourse-topic-state-back-stack state)))))
        (push source-id (discourse-topic-state-back-stack state))))
    (if target-id
        (discourse-topic--focus-post-id view target-id)
      (setf (discourse-topic-state-target-post-number state) post-number)
      (if (discourse-topic-state-request-token state)
          (message "Post #%d will open after the current page loads"
                   post-number)
        (discourse-topic--continue-target view state)))))

(defun discourse-topic-jump-back ()
  "Return to the previous intra-topic post anchor."
  (interactive)
  (let* ((view (appkit-current-view))
         (state (discourse-topic--state view))
         target)
    (while (and (discourse-topic-state-back-stack state) (null target))
      (let ((candidate (pop (discourse-topic-state-back-stack state))))
        (when (and (gethash candidate
                            (discourse-topic-state-loaded-ids state))
                   (discourse-topic--canonical-post state candidate))
          (setq target candidate))))
    (if target
        (discourse-topic--focus-post-id view target)
      (user-error "No previous Discourse post anchor"))))

(defun discourse-topic--continue-target (view state)
  "Focus or continue loading STATE's target post in VIEW."
  (when-let* ((post-number (discourse-topic-state-target-post-number state)))
    (if-let* ((post-id
               (discourse-topic--post-id-for-number state post-number)))
        (progn
          (setf (discourse-topic-state-target-post-number state) nil)
          (appkit-view-enqueue-event view (list :position post-id))
          (appkit-request-sync view :part 'entries :position t))
      (if (discourse-topic-state-exhausted-p state)
          (progn
            (setf (discourse-topic-state-target-post-number state) nil)
            (message "Post #%d is not available in this topic" post-number))
        (discourse-topic--request view 'posts)))))

(defun discourse-topic--handle-snapshot
    (view state token phase snapshot)
  "Install validated SNAPSHOT into VIEW STATE for PHASE."
  (when (discourse-topic--request-current-p view state token)
    (condition-case error-data
        (let* ((topic (discourse-topic-snapshot-topic snapshot))
               (topic-id (discourse-state-id (gethash "id" topic)))
               (stream (discourse-topic-snapshot-stream snapshot))
               (posts (discourse-topic-snapshot-posts snapshot))
               (canonical (discourse-account-state
                           (discourse-topic-state-account state))))
          (unless (equal topic-id (discourse-topic-state-topic-id state))
            (error "Discourse returned a different topic"))
          (discourse-state-merge-topic canonical topic)
          (let ((loaded
                 (if (eq phase 'refresh)
                     (discourse-topic--retain-loaded state stream)
                   (make-hash-table :test #'equal))))
            (dolist (post posts)
              (let ((post-topic-id
                     (discourse-state-id
                      (discourse-topic--field post "topic_id"))))
                (unless (equal topic-id post-topic-id)
                  (error "Discourse returned a post from another topic")))
              (discourse-topic--observe-post-author canonical post)
              (let ((post-id (discourse-state-merge-post canonical post)))
                (puthash post-id t loaded)))
            (setf (discourse-topic-state-stream state) stream
                  (discourse-topic-state-loaded-ids state) loaded
                  (discourse-topic-state-phase state) 'ready
                  (discourse-topic-state-message state) nil
                  (discourse-topic-state-request-token state) nil
                  (discourse-topic-state-retry-phase state) nil
                  (discourse-topic-state-retry-post-ids state) nil
                  (discourse-topic-state-loaded-p state) t
                  (discourse-topic-state-exhausted-p state)
                  (cl-every (lambda (id) (gethash id loaded)) stream)))
          (discourse-topic--update-buffer-name view state)
          (appkit-view-enqueue-event
           view (list :position (if (eq phase 'initial) 'first 'preserve)))
          (appkit-request-sync view :structure t :part 'frame :position t)
          (if (discourse-topic-state-target-post-number state)
              (discourse-topic--continue-target view state)
            (message "Loaded %d/%d Discourse posts"
                     (hash-table-count
                      (discourse-topic-state-loaded-ids state))
                     (length stream))))
      (error
       (discourse-topic--handle-error
        view state token phase nil
        (discourse-http-result-create
         :ok-p nil
         :failure
         (discourse-http-failure-create
          :kind 'invalid-response
          :message (error-message-string error-data))))))))

(defun discourse-topic--handle-post-page
    (view state token requested posts)
  "Install POSTS requested by REQUESTED into VIEW STATE."
  (when (discourse-topic--request-current-p view state token)
    (condition-case error-data
        (let* ((requested-table (make-hash-table :test #'equal))
               (canonical
                (discourse-account-state
                 (discourse-topic-state-account state)))
               (topic-id (discourse-topic-state-topic-id state)))
          (dolist (post-id requested)
            (puthash post-id t requested-table))
          (dolist (post posts)
            (let ((post-id (discourse-topic--post-id post))
                  (post-topic-id
                   (discourse-state-id
                    (discourse-topic--field post "topic_id"))))
              (unless (and (gethash post-id requested-table)
                           (equal topic-id post-topic-id))
                (error "Discourse returned an unexpected post page"))
              (discourse-topic--observe-post-author canonical post)
              (discourse-state-merge-post canonical post)))
          ;; Missing IDs can represent posts that became unavailable.  Mark
          ;; every requested stream slot consumed so pagination cannot loop.
          (dolist (post-id requested)
            (puthash post-id t (discourse-topic-state-loaded-ids state)))
          (setf (discourse-topic-state-phase state) 'ready
                (discourse-topic-state-message state) nil
                (discourse-topic-state-request-token state) nil
                (discourse-topic-state-retry-phase state) nil
                (discourse-topic-state-retry-post-ids state) nil
                (discourse-topic-state-exhausted-p state)
                (cl-every
                 (lambda (id)
                   (gethash id (discourse-topic-state-loaded-ids state)))
                 (discourse-topic-state-stream state)))
          (appkit-view-enqueue-event view (list :position 'preserve))
          (appkit-request-sync view :structure t :part 'frame :position t)
          (when (discourse-topic-state-target-post-number state)
            (discourse-topic--continue-target view state)))
      (error
       (discourse-topic--handle-error
        view state token 'posts requested
        (discourse-http-result-create
         :ok-p nil
         :failure
         (discourse-http-failure-create
          :kind 'invalid-response
          :message (error-message-string error-data))))))))

(defun discourse-topic--cancel-request (view)
  "Cancel VIEW's active topic transport."
  (let* ((state (discourse-topic--state view))
         (request
          (gethash discourse-topic--request-key
                   (appkit-view-request-table view))))
    (when (discourse-topic-state-request-token state)
      (setf (discourse-topic-state-request-token state) nil
            (discourse-topic-state-phase state)
            (if (discourse-topic-state-loaded-p state) 'ready 'initial)))
    (when request
      (remhash discourse-topic--request-key
               (appkit-view-request-table view))
      (discourse-http-cancel request))))

(defun discourse-topic--next-post-ids (state)
  "Return next unloaded stream IDs for topic STATE."
  (cl-loop for post-id in (discourse-topic-state-stream state)
           unless (gethash post-id (discourse-topic-state-loaded-ids state))
           collect post-id into result
           when (= (length result) discourse-topic-post-page-size)
           return result
           finally return result))

(defun discourse-topic--maybe-auto-load
    (view _window position end)
  "Load VIEW's next post page when POSITION approaches END."
  (when (and (appkit-view-live-p view)
             (numberp discourse-scroll-load-threshold)
             (appkit-scroll-near-end-p
              position end discourse-scroll-load-threshold))
    (let ((state (discourse-topic--state view)))
      (when (and (discourse-topic-state-loaded-p state)
                 (eq (discourse-topic-state-phase state) 'ready)
                 (null (discourse-topic-state-request-token state))
                 (not (discourse-topic-state-exhausted-p state))
                 (discourse-topic--next-post-ids state))
        (discourse-topic--request view 'posts)))))

(defun discourse-topic--install-scroll-observer (view)
  "Install VIEW's lifecycle-owned post pagination observer."
  (setq-local
   discourse-topic--scroll-observer
   (appkit-scroll-observer-install
    view
    :end-function
    (lambda (window position end)
      (discourse-topic--maybe-auto-load
       view window position end)))))

(defun discourse-topic--request (view phase &optional retry-post-ids)
  "Start topic VIEW request for PHASE.
RETRY-POST-IDS, when non-nil, is the exact failed post page to replay."
  (unless (memq phase '(initial refresh posts))
    (error "Invalid Discourse topic request phase"))
  (let* ((state (discourse-topic--state view))
         (post-ids
          (and (eq phase 'posts)
               (copy-sequence
                (or retry-post-ids
                    (discourse-topic--next-post-ids state)))))
         (token (list phase post-ids (gensym "discourse-topic-")))
         request callback-ran-p)
    (when (and (eq phase 'posts) (null post-ids))
      (setf (discourse-topic-state-exhausted-p state) t)
      (user-error "No more Discourse posts"))
    (discourse-topic--cancel-request view)
    (setf (discourse-topic-state-request-token state) token
          (discourse-topic-state-phase state) phase
          (discourse-topic-state-message state) nil
          (discourse-topic-state-retry-phase state) nil
          (discourse-topic-state-retry-post-ids state) nil)
    (appkit-request-sync view :part 'frame :position t)
    (setq
     request
     (if (eq phase 'posts)
         (discourse-api-topic-posts
          (discourse-topic-state-account state)
          (discourse-topic-state-topic-id state)
          post-ids
          (lambda (result)
            (setq callback-ran-p t)
            (discourse-topic--retire-request view state token)
            (if (discourse-http-result-ok-p result)
                (discourse-topic--handle-post-page
                 view state token post-ids
                 (discourse-http-result-data result))
              (discourse-topic--handle-error
               view state token phase post-ids result)))
          :owner view)
       (discourse-api-topic
        (discourse-topic-state-account state)
        (discourse-topic-state-topic-id state)
        (lambda (result)
          (setq callback-ran-p t)
          (discourse-topic--retire-request view state token)
          (if (discourse-http-result-ok-p result)
              (discourse-topic--handle-snapshot
               view state token phase
               (discourse-http-result-data result))
            (discourse-topic--handle-error
             view state token phase nil result)))
        :owner view)))
    (when (and request
               (not callback-ran-p)
               (discourse-topic--request-current-p view state token))
      (puthash discourse-topic--request-key request
               (appkit-view-request-table view)))
    request))

(defun discourse-topic-can-reply-p ()
  "Return non-nil when the server permits replying to the current topic."
  (condition-case nil
      (let ((state (discourse-topic--state)))
        (discourse-compose-reply-allowed-p
         (discourse-topic-state-account state)
         (discourse-topic-state-topic-id state)))
    (error nil)))

(defun discourse-topic--post-at-point (state)
  "Return STATE's canonical post represented at point, or nil."
  (let ((post-id
         (or (get-text-property (point) discourse-topic-post-id-property)
             (and (> (point) (point-min))
                  (get-text-property
                   (1- (point)) discourse-topic-post-id-property)))))
    (and post-id (discourse-topic--canonical-post state post-id))))

(defun discourse-topic-compose-reply (&optional topic-level-p)
  "Compose a reply to the post at point.
With TOPIC-LEVEL-P, compose an unscoped reply to the topic."
  (interactive "P")
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic")))
         (state (discourse-topic--state view))
         (post (and (not topic-level-p)
                    (discourse-topic--post-at-point state)))
         (post-number
          (and post (discourse-topic--field post "post_number")))
         (username (and post (discourse-topic--sender-name post))))
    (discourse-compose-reply
     (discourse-topic-state-account state)
     (discourse-topic-state-topic-id state)
     :source-view view
     :reply-to-post-number
     (and (integerp post-number) post-number)
     :reply-to-username username
     :select t)))

(defun discourse-topic-refresh-to-post-number (post-number)
  "Refresh the current topic and focus accepted POST-NUMBER."
  (unless (and (integerp post-number) (> post-number 0))
    (error "Discourse accepted an invalid post number"))
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic")))
         (state (discourse-topic--state view)))
    (setf (discourse-topic-state-target-post-number state) post-number)
    (discourse-topic--request view 'refresh)))

(defun discourse-topic-refresh ()
  "Refresh the current Discourse topic snapshot."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic")))
         (state (discourse-topic--state view)))
    (discourse-topic--request
     view (if (discourse-topic-state-loaded-p state) 'refresh 'initial))))

(defun discourse-topic-retry-available-p ()
  "Return non-nil when the current topic has a failed request."
  (condition-case nil
      (let ((state (discourse-topic--state)))
        (and (eq (discourse-topic-state-phase state) 'error)
             (discourse-topic-state-retry-phase state)
             (null (discourse-topic-state-request-token state))))
    (error nil)))

(defun discourse-topic-retry ()
  "Retry the current topic's exact failed request."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic")))
         (state (discourse-topic--state view))
         (phase (discourse-topic-state-retry-phase state))
         (post-ids (discourse-topic-state-retry-post-ids state)))
    (unless (and (eq (discourse-topic-state-phase state) 'error)
                 phase
                 (or (not (eq phase 'posts)) post-ids)
                 (null (discourse-topic-state-request-token state)))
      (user-error "No failed Discourse topic request to retry"))
    (discourse-topic--request view phase post-ids)))


(defun discourse-topic-open-latest ()
  "Open the current topic's account Latest view."
  (interactive)
  (let ((state (discourse-topic--state)))
    (discourse-topic-list-open-latest
     (discourse-topic-state-account state) t)))

(defvar-keymap discourse-topic-mode-map
  :parent special-mode-map
  "g" #'discourse-topic-refresh
  "n" #'appkit-discussion-next-entry
  "R" #'discourse-topic-retry
  "r" #'discourse-topic-compose-reply
  "b" #'discourse-topic-open-latest
  "l" #'discourse-topic-jump-back
  "p" #'appkit-discussion-previous-entry
  "?" #'discourse-topic-transient
  "q" #'quit-window)

(define-derived-mode discourse-topic-mode special-mode "Discourse-Topic"
  "Major mode for a projected Discourse topic."
  (setq-local truncate-lines nil
              header-line-format
              '(:eval (discourse-topic--header-line))))

(defun discourse-topic--setup (view)
  "Initialize newly attached topic VIEW."
  (discourse-topic--state view)
  (appkit-projection-ensure
   view
   :printer #'discourse-topic--print-row
   :anchor-property appkit-discussion-key-property
   :no-separator-p t)
  (discourse-topic--install-scroll-observer view)
  (appkit-view-enable-responsive-geometry view)
  (appkit-view-enqueue-event view (list :position 'first))
  (appkit-invalidate view :structure t :parts '(frame entries geometry))
  (appkit-sync-invalidations view)
  (discourse-topic--request-site-metadata view)
  (discourse-topic--request view 'initial))

(defun discourse-topic-open
    (account topic-id &optional select post-number)
  "Open ACCOUNT TOPIC-ID and optionally SELECT it at POST-NUMBER."
  (unless (and (discourse-account-p account)
               (appkit-app-live-p (discourse-account-app account)))
    (user-error "Discourse account is not running"))
  (let* ((topic-id (discourse-state-id topic-id))
         (app (discourse-account-app account))
         (view-id (list 'topic topic-id))
         (existing (appkit-view-for-id app view-id))
         (state
          (if existing
              (appkit-view-state existing)
            (discourse-topic-state-create
             :account account
             :topic-id topic-id
             :stream nil
             :loaded-ids (make-hash-table :test #'equal)
             :phase 'initial
             :loaded-p nil
             :exhausted-p nil
             :target-post-number post-number)))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode #'discourse-topic-mode
           :buffer-name
           (format "*Discourse: %s · t/%s*"
                   (discourse-account-origin account) topic-id)
           :state state
           :sync-function #'discourse-topic--sync
           :parts '(frame entries geometry)
           :position-policy 'semantic
           :setup #'discourse-topic--setup
           :select select)))
    (when (and existing post-number)
      (setf (discourse-topic-state-target-post-number state) post-number)
      (unless (discourse-topic-state-request-token state)
        (discourse-topic--continue-target view state)))
    (appkit-view-buffer view)))

(provide 'discourse-topic)

;;; discourse-topic.el ends here
