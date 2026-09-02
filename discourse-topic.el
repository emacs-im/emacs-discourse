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
(require 'appkit-invalidation)
(require 'appkit-markup)
(require 'appkit-markup-ui)
(require 'appkit-position)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'appkit-view)
(require 'discourse-api)
(require 'discourse-customize)
(require 'discourse-markup)
(require 'discourse-runtime)
(require 'discourse-state)
(declare-function discourse-topic-transient "discourse-transient" ())


(defconst discourse-topic--request-key 'posts
  "View request-table key for the active topic request.")

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
  target-post-number)

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

(defun discourse-topic--post-context (post)
  "Return contextual label for POST."
  (let ((number (discourse-topic--field post "post_number"))
        (reply-to (discourse-topic--field post "reply_to_post_number")))
    (concat
     (if (integerp number) (format "#%d" number) "post")
     (if (integerp reply-to) (format " · replying to #%d" reply-to) ""))))

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

(defun discourse-topic--insert-markup-object (node)
  "Insert one Discourse provider object NODE natively."
  (let* ((value (discourse-topic--markup-value node))
         (kind (and value (discourse-markup-provider-object-kind value)))
         (data (and value (discourse-markup-provider-object-data value)))
         (url (and (listp data) (plist-get data :url))))
    (pcase kind
      ((or 'image 'media 'onebox 'lazy-video)
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
    (appkit-markup-ui-insert-document
     document
     :prefix prefix
     :properties properties
     :final-newline-p t
     :interactive-p t
     :link-action
     (lambda (url) (discourse-topic--link-action account url))
     :object-inserter #'discourse-topic--insert-markup-object)))

(defun discourse-topic--entry (view post)
  "Return Appkit discussion entry for POST in VIEW."
  (let* ((id (discourse-topic--post-id post))
         (number (discourse-topic--field post "post_number"))
         (properties
          (list discourse-topic-post-id-property id
                discourse-topic-post-number-property number
                'discourse-post post)))
    (appkit-discussion-entry-create
     :key id
     :avatar-fallback "@"
     :context (discourse-topic--post-context post)
     :context-face 'shadow
     :heading
     (discourse-topic--string
      (or (discourse-topic--field post "display_username")
          (discourse-topic--field post "username"))
      "(deleted user)")
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
     :width (or (appkit-view-window-fill-column) 80)
     :avatar-p nil)))

(defun discourse-topic--project (state)
  "Project loaded posts from topic STATE."
  (appkit-projection-project
   (discourse-topic--ordered-posts state)
   #'discourse-topic--post-id
   :dependencies-function
   (lambda (post)
     (list (list :post (discourse-topic--post-id post))))))

(defun discourse-topic--header (state)
  "Return generated header for topic STATE."
  (let* ((topic (discourse-topic--canonical-topic state))
         (title
          (discourse-topic--string
           (discourse-topic--field topic "title")
           (format "Topic %s" (discourse-topic-state-topic-id state))))
         (category (discourse-topic--field topic "category_id"))
         (tags
          (condition-case nil
              (discourse-state-sequence-list
               (discourse-topic--field topic "tags"))
            (error nil))))
    (concat
     (propertize title 'face '(:height 1.2 :weight bold))
     "\n"
     (propertize
      (string-join
       (delq nil
             (list
              (and category (format "category %s" category))
              (and tags
                   (string-join
                    (cl-loop for tag in tags
                             when (stringp tag)
                             collect (concat "#" tag))
                    " "))))
       " · ")
      'face 'shadow)
     "\n\n")))

(defun discourse-topic--footer (state)
  "Return generated footer for topic STATE."
  (let ((loaded (hash-table-count
                 (discourse-topic-state-loaded-ids state)))
        (total (length (discourse-topic-state-stream state))))
    (concat
     "\n"
     (pcase (discourse-topic-state-phase state)
       ('initial "Loading topic…")
       ('refresh "Refreshing topic…")
       ('posts "Loading more posts…")
       ('error
        (format "Unable to load topic: %s"
                (or (discourse-topic-state-message state) "unknown error")))
       (_
        (concat
         (format "%d/%d posts loaded" loaded total)
         (if (discourse-topic-state-exhausted-p state)
             " · complete"
           " · N loads more"))))
     "\n")))

(defun discourse-topic--position-intent (events)
  "Return effective semantic position intent from EVENTS."
  (or (cl-loop for event in events
               when (eq (plist-get event :position) 'first)
               return 'first)
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
    (appkit-view-acknowledge-events view event-count)))

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

(defun discourse-topic--handle-error (view state token result)
  "Install failed RESULT when TOKEN owns STATE in VIEW."
  (when (discourse-topic--request-current-p view state token)
    (setf (discourse-topic-state-phase state) 'error
          (discourse-topic-state-message state)
          (discourse-topic--failure-message result)
          (discourse-topic-state-request-token state) nil)
    (appkit-request-sync view :part 'frame :position t)
    (message "%s" (discourse-topic-state-message state))))

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
       (format "*Discourse: %s*"
               (truncate-string-to-width title 60 nil nil "…"))
       t))))

(defun discourse-topic--post-id-for-number (state post-number)
  "Return loaded post ID in STATE for POST-NUMBER."
  (cl-loop for post in (discourse-topic--ordered-posts state)
           when (= post-number
                   (or (discourse-topic--field post "post_number") -1))
           return (discourse-topic--post-id post)))

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
              (let ((post-id (discourse-state-merge-post canonical post)))
                (puthash post-id t loaded)))
            (setf (discourse-topic-state-stream state) stream
                  (discourse-topic-state-loaded-ids state) loaded
                  (discourse-topic-state-phase state) 'ready
                  (discourse-topic-state-message state) nil
                  (discourse-topic-state-request-token state) nil
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
        view state token
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
              (discourse-state-merge-post canonical post)))
          ;; Missing IDs can represent posts that became unavailable.  Mark
          ;; every requested stream slot consumed so pagination cannot loop.
          (dolist (post-id requested)
            (puthash post-id t (discourse-topic-state-loaded-ids state)))
          (setf (discourse-topic-state-phase state) 'ready
                (discourse-topic-state-message state) nil
                (discourse-topic-state-request-token state) nil
                (discourse-topic-state-exhausted-p state)
                (cl-every
                 (lambda (id)
                   (gethash id (discourse-topic-state-loaded-ids state)))
                 (discourse-topic-state-stream state)))
          (appkit-view-enqueue-event view (list :position 'preserve))
          (appkit-request-sync view :structure t :part 'frame :position t)
          (if (discourse-topic-state-target-post-number state)
              (discourse-topic--continue-target view state)
            (message "Loaded %d more Discourse posts" (length posts))))
      (error
       (discourse-topic--handle-error
        view state token
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

(defun discourse-topic--request (view phase)
  "Start topic VIEW request for PHASE."
  (unless (memq phase '(initial refresh posts))
    (error "Invalid Discourse topic request phase"))
  (let* ((state (discourse-topic--state view))
         (post-ids (and (eq phase 'posts)
                        (discourse-topic--next-post-ids state)))
         (token (list phase (gensym "discourse-topic-")))
         request callback-ran-p)
    (when (and (eq phase 'posts) (null post-ids))
      (setf (discourse-topic-state-exhausted-p state) t)
      (user-error "No more Discourse posts"))
    (discourse-topic--cancel-request view)
    (setf (discourse-topic-state-request-token state) token
          (discourse-topic-state-phase state) phase
          (discourse-topic-state-message state) nil)
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
              (discourse-topic--handle-error view state token result)))
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
            (discourse-topic--handle-error view state token result)))
        :owner view)))
    (when (and request
               (not callback-ran-p)
               (discourse-topic--request-current-p view state token))
      (puthash discourse-topic--request-key request
               (appkit-view-request-table view)))
    request))

(defun discourse-topic-refresh ()
  "Refresh the current Discourse topic snapshot."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic")))
         (state (discourse-topic--state view)))
    (discourse-topic--request
     view (if (discourse-topic-state-loaded-p state) 'refresh 'initial))))

(defun discourse-topic-load-more ()
  "Load the next post-stream page in the current topic."
  (interactive)
  (let ((view (or (appkit-current-view)
                  (user-error "No live Discourse topic"))))
    (discourse-topic--request view 'posts)))

(defvar-keymap discourse-topic-mode-map
  :parent special-mode-map
  "g" #'discourse-topic-refresh
  "N" #'discourse-topic-load-more
  "n" #'appkit-discussion-next-entry
  "p" #'appkit-discussion-previous-entry
  "?" #'discourse-topic-transient
  "q" #'quit-window)

(define-derived-mode discourse-topic-mode special-mode "Discourse-Topic"
  "Major mode for a projected Discourse topic."
  (setq-local truncate-lines nil))

(defun discourse-topic--setup (view)
  "Initialize newly attached topic VIEW."
  (discourse-topic--state view)
  (appkit-projection-ensure
   view
   :printer #'discourse-topic--print-row
   :anchor-property appkit-discussion-key-property
   :no-separator-p t)
  (appkit-view-enable-responsive-geometry view)
  (appkit-view-enqueue-event view (list :position 'first))
  (appkit-invalidate view :structure t :parts '(frame entries geometry))
  (appkit-sync-invalidations view)
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
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode #'discourse-topic-mode
           :buffer-name (format "*Discourse Topic %s*" topic-id)
           :state
           (discourse-topic-state-create
            :account account
            :topic-id topic-id
            :stream nil
            :loaded-ids (make-hash-table :test #'equal)
            :phase 'initial
            :loaded-p nil
            :exhausted-p nil
            :target-post-number post-number)
           :sync-function #'discourse-topic--sync
           :parts '(frame entries geometry)
           :position-policy 'semantic
           :setup #'discourse-topic--setup
           :select select)))
    (when (and existing post-number)
      (let ((state (discourse-topic--state view)))
        (setf (discourse-topic-state-target-post-number state) post-number)
        (unless (discourse-topic-state-request-token state)
          (discourse-topic--continue-target view state))))
    (appkit-view-buffer view)))

(provide 'discourse-topic)

;;; discourse-topic.el ends here
