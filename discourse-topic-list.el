;;; discourse-topic-list.el --- Discourse topic list views -*- lexical-binding: t; -*-

;;; Commentary:

;; Stable-key Appkit projection for anonymous Latest pages and the server-owned
;; `more_topics_url' cursor.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'appkit-view)
(require 'discourse-api)
(require 'discourse-runtime)
(require 'discourse-state)

(declare-function discourse-topic-open
                  "discourse-topic" (account topic-id &optional select post-number))
(declare-function discourse-topic-list-transient
                  "discourse-transient" ())

(defconst discourse-topic-list--request-key 'topics
  "View request-table key for the active topic page request.")

(defconst discourse-topic-list-id-property 'discourse-topic-id
  "Text property carrying a stable Discourse topic ID.")

(cl-defstruct (discourse-topic-list-state
               (:constructor discourse-topic-list-state-create)
               (:copier nil))
  account
  topics
  more-url
  phase
  message
  request-token
  loaded-p
  exhausted-p)

(defun discourse-topic-list--state (&optional view)
  "Return validated topic-list state for VIEW or the current view."
  (let* ((view (or view (appkit-current-view)))
         (state (and (appkit-view-live-p view) (appkit-view-state view))))
    (unless (and (discourse-topic-list-state-p state)
                 (discourse-account-p
                  (discourse-topic-list-state-account state)))
      (error "Current view has no Discourse topic-list state"))
    state))

(defun discourse-topic-list--field (topic key &optional default)
  "Return TOPIC string KEY or DEFAULT."
  (if (hash-table-p topic) (gethash key topic default) default))

(defun discourse-topic-list--string (value &optional fallback)
  "Return property-free VALUE when it is a string, else FALLBACK."
  (if (stringp value) (substring-no-properties value) (or fallback "")))

(defun discourse-topic-list--id (topic)
  "Return TOPIC's stable remote ID."
  (discourse-state-id (discourse-topic-list--field topic "id")))

(defun discourse-topic-list--format-time (topic)
  "Return compact activity time for TOPIC."
  (let ((value (or (discourse-topic-list--field topic "bumped_at")
                   (discourse-topic-list--field topic "last_posted_at")
                   (discourse-topic-list--field topic "created_at"))))
    (if (not (stringp value))
        ""
      (condition-case nil
          (format-time-string "%m-%d %H:%M" (date-to-time value))
        (error value)))))

(defun discourse-topic-list--tags (topic)
  "Return compact tag text for TOPIC."
  (let ((tags
         (condition-case nil
             (discourse-state-sequence-list
              (discourse-topic-list--field topic "tags"))
           (error nil))))
    (string-join
     (cl-loop for tag in tags
              repeat 3
              when (stringp tag)
              collect (concat "#" (substring-no-properties tag)))
     " ")))

(defun discourse-topic-list--stats (topic)
  "Return compact activity statistics for TOPIC."
  (let ((posts (discourse-topic-list--field topic "posts_count" 0))
        (views (discourse-topic-list--field topic "views" 0))
        (likes (discourse-topic-list--field topic "like_count" 0))
        (tags (discourse-topic-list--tags topic)))
    (string-join
     (delq nil
           (list
            (and (integerp posts) (format "%d posts" posts))
            (and (integerp views) (format "%d views" views))
            (and (integerp likes) (> likes 0) (format "%d likes" likes))
            (unless (string-empty-p tags) tags)))
     " · ")))

(defun discourse-topic-list--row-model (topic)
  "Return Appkit one-line model for TOPIC."
  (let* ((id (discourse-topic-list--id topic))
         (category-id
          (condition-case nil
              (discourse-state-id
               (discourse-topic-list--field topic "category_id"))
            (error "?")))
         (unseen
          (or (eq t (discourse-topic-list--field topic "unseen"))
              (eq t (discourse-topic-list--field topic "new_posts")))))
    (appkit-view-one-line-row-create
     :context (format "c:%s" category-id)
     :context-open "["
     :context-close "]"
     :context-trail (and unseen "new")
     :context-trail-face 'warning
     :preview
     (appkit-ui-one-line-preview-create
      :label
      (discourse-topic-list--string
       (discourse-topic-list--field topic "title") "(untitled topic)")
      :separator " — "
      :text (discourse-topic-list--stats topic)
      :label-face (and unseen 'bold))
     :time (discourse-topic-list--format-time topic)
     :time-face 'shadow
     :line-properties
     (list discourse-topic-list-id-property id
           'discourse-topic topic)
     :mouse-face 'highlight)))

(defun discourse-topic-list--print-row (row)
  "Insert projected topic ROW."
  (appkit-view-insert-one-line-row
   (discourse-topic-list--row-model
    (appkit-projection-row-payload row))
   :indent 1
   :width (or (appkit-view-responsive-width 1) fill-column 100)
   :context-width-spec '(0.16 7 16)
   :time-slot-width 11))

(defun discourse-topic-list--project (state)
  "Project topic-list STATE into stable rows."
  (appkit-projection-project
   (discourse-topic-list-state-topics state)
   #'discourse-topic-list--id))

(defun discourse-topic-list--header (state)
  "Return generated header for topic-list STATE."
  (concat
   (propertize
    (format "Latest · %s"
            (discourse-account-origin
             (discourse-topic-list-state-account state)))
    'face '(:height 1.2 :weight bold))
   "\n\n"))

(defun discourse-topic-list--footer (state)
  "Return generated footer for topic-list STATE."
  (let ((count (length (discourse-topic-list-state-topics state))))
    (concat
     "\n"
     (pcase (discourse-topic-list-state-phase state)
       ('initial "Loading topics…")
       ('refresh "Refreshing topics…")
       ('older "Loading more topics…")
       ('error
        (format "Unable to load topics: %s"
                (or (discourse-topic-list-state-message state)
                    "unknown error")))
       (_
        (concat
         (format "%d topic%s" count (if (= count 1) "" "s"))
         (if (discourse-topic-list-state-exhausted-p state)
             " · end of list"
           " · N loads more"))))
     "\n")))

(defun discourse-topic-list--position-intent (events)
  "Return effective semantic position intent from EVENTS."
  (or (cl-loop for event in events
               when (eq (plist-get event :position) 'first)
               return 'first)
      (cl-loop for event in (reverse events)
               for position = (plist-get event :position)
               when position return position)
      'preserve))

(defun discourse-topic-list--sync (view invalidations)
  "Synchronize topic-list VIEW from INVALIDATIONS."
  (let* ((state (discourse-topic-list--state view))
         (events (appkit-view-pending-events-snapshot view))
         (event-count (length events))
         (parts (appkit-invalidations-parts invalidations))
         (entry-keys (appkit-invalidations-entry-keys invalidations))
         (geometry-p (memq 'geometry parts))
         (reconcile-p
          (or geometry-p
              (appkit-invalidations-structure-p invalidations)
              (memq 'entries parts)
              entry-keys))
         (rows (and reconcile-p (discourse-topic-list--project state))))
    (appkit-projection-sync
     view rows
     :header (discourse-topic-list--header state)
     :footer (discourse-topic-list--footer state)
     :force-keys
     (if geometry-p
         (mapcar #'appkit-projection-row-key rows)
       entry-keys)
     :position (discourse-topic-list--position-intent events)
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)))

(defun discourse-topic-list--request-current-p (view state token)
  "Return non-nil when TOKEN may update STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq token (discourse-topic-list-state-request-token state))))

(defun discourse-topic-list--retire-request (view state token)
  "Retire VIEW's request table entry owned by TOKEN."
  (when (discourse-topic-list--request-current-p view state token)
    (remhash discourse-topic-list--request-key
             (appkit-view-request-table view))))

(defun discourse-topic-list--failure-message (result)
  "Return presentation message for failed HTTP RESULT."
  (let ((failure (discourse-http-result-failure result)))
    (if (discourse-http-failure-p failure)
        (discourse-http-failure-message failure)
      "Unknown Discourse response failure")))

(defun discourse-topic-list--handle-error (view state token result)
  "Install failed RESULT when TOKEN still owns STATE in VIEW."
  (when (discourse-topic-list--request-current-p view state token)
    (setf (discourse-topic-list-state-phase state) 'error
          (discourse-topic-list-state-message state)
          (discourse-topic-list--failure-message result)
          (discourse-topic-list-state-request-token state) nil)
    (appkit-request-sync view :part 'frame :position t)
    (message "%s" (discourse-topic-list-state-message state))))

(defun discourse-topic-list--new-topics (current candidates)
  "Return CANDIDATES whose IDs are absent from CURRENT."
  (let ((seen (make-hash-table :test #'equal)) result)
    (dolist (topic current)
      (puthash (discourse-topic-list--id topic) t seen))
    (dolist (topic candidates (nreverse result))
      (let ((id (discourse-topic-list--id topic)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push topic result))))))

(defun discourse-topic-list--handle-success
    (view state token phase page)
  "Install validated PAGE into VIEW STATE for PHASE."
  (when (discourse-topic-list--request-current-p view state token)
    (condition-case error-data
        (let* ((topics (discourse-topic-page-topics page))
               (current (discourse-topic-list-state-topics state))
               (installed
                (pcase phase
                  ((or 'initial 'refresh)
                   (append topics
                           (discourse-topic-list--new-topics topics current)))
                  ('older
                   (append current
                           (discourse-topic-list--new-topics current topics)))
                  (_ (error "Invalid topic request phase")))))
          (dolist (topic topics)
            (discourse-state-merge-topic
             (discourse-account-state
              (discourse-topic-list-state-account state))
             topic))
          (setf (discourse-topic-list-state-topics state) installed
                (discourse-topic-list-state-more-url state)
                (discourse-topic-page-more-url page)
                (discourse-topic-list-state-phase state) 'ready
                (discourse-topic-list-state-message state) nil
                (discourse-topic-list-state-request-token state) nil
                (discourse-topic-list-state-loaded-p state) t
                (discourse-topic-list-state-exhausted-p state)
                (null (discourse-topic-page-more-url page)))
          (appkit-view-enqueue-event
           view (list :position (if (eq phase 'initial) 'first 'preserve)))
          (appkit-request-sync view :structure t :part 'frame :position t)
          (message "Loaded %d Discourse topics" (length topics)))
      (error
       (let ((result
              (discourse-http-result-create
               :ok-p nil
               :failure
               (discourse-http-failure-create
                :kind 'invalid-response
                :message (error-message-string error-data)))))
         (discourse-topic-list--handle-error view state token result))))))

(defun discourse-topic-list--cancel-request (view)
  "Cancel VIEW's active topic request."
  (let* ((state (discourse-topic-list--state view))
         (request
          (gethash discourse-topic-list--request-key
                   (appkit-view-request-table view))))
    (when (discourse-topic-list-state-request-token state)
      (setf (discourse-topic-list-state-request-token state) nil
            (discourse-topic-list-state-phase state)
            (if (discourse-topic-list-state-loaded-p state)
                'ready
              'initial)))
    (when request
      (remhash discourse-topic-list--request-key
               (appkit-view-request-table view))
      (discourse-http-cancel request))))

(defun discourse-topic-list--request (view phase)
  "Start topic-list VIEW request for PHASE."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Discourse topic request phase"))
  (let* ((state (discourse-topic-list--state view))
         (endpoint
          (if (eq phase 'older)
              (or (discourse-topic-list-state-more-url state)
                  (user-error "No more Discourse topics"))
            "/latest.json"))
         (token (list phase (gensym "discourse-topics-")))
         request callback-ran-p)
    (when (and (eq phase 'older)
               (discourse-topic-list-state-exhausted-p state))
      (user-error "No more Discourse topics"))
    (discourse-topic-list--cancel-request view)
    (setf (discourse-topic-list-state-request-token state) token
          (discourse-topic-list-state-phase state) phase
          (discourse-topic-list-state-message state) nil)
    (appkit-request-sync view :part 'frame :position t)
    (setq request
          (discourse-api-topic-page
           (discourse-topic-list-state-account state)
           (lambda (result)
             (setq callback-ran-p t)
             (discourse-topic-list--retire-request view state token)
             (if (discourse-http-result-ok-p result)
                 (discourse-topic-list--handle-success
                  view state token phase
                  (discourse-http-result-data result))
               (discourse-topic-list--handle-error
                view state token result)))
           :endpoint endpoint
           :owner view))
    (when (and request
               (not callback-ran-p)
               (discourse-topic-list--request-current-p view state token))
      (puthash discourse-topic-list--request-key request
               (appkit-view-request-table view)))
    request))

(defun discourse-topic-list-refresh ()
  "Refresh the current Discourse topic list."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic list")))
         (state (discourse-topic-list--state view)))
    (discourse-topic-list--request
     view
     (if (discourse-topic-list-state-loaded-p state) 'refresh 'initial))))

(defun discourse-topic-list-load-more ()
  "Load the next server-provided page in the current topic list."
  (interactive)
  (let ((view (or (appkit-current-view)
                  (user-error "No live Discourse topic list"))))
    (discourse-topic-list--request view 'older)))

(defun discourse-topic-list--row-positions ()
  "Return start positions of projected topic rows."
  (let ((position (point-min))
        (limit (point-max))
        previous
        result)
    (while (< position limit)
      (let ((value (get-text-property
                    position discourse-topic-list-id-property)))
        (when (and value (not (equal value previous)))
          (push position result))
        (setq previous value
              position
              (or (next-single-property-change
                   position discourse-topic-list-id-property nil limit)
                  limit))))
    (nreverse result)))

(defun discourse-topic-list-next ()
  "Move to the next projected topic row."
  (interactive)
  (if-let* ((target
             (seq-find (lambda (position) (> position (point)))
                       (discourse-topic-list--row-positions))))
      (goto-char target)
    (user-error "No later Discourse topic")))

(defun discourse-topic-list-previous ()
  "Move to the previous projected topic row."
  (interactive)
  (let* ((current-id
          (get-text-property (point) discourse-topic-list-id-property))
         (candidates
          (cl-remove-if
           (lambda (position)
             (or (>= position (point))
                 (and current-id
                      (equal current-id
                             (get-text-property
                              position discourse-topic-list-id-property)))))
           (discourse-topic-list--row-positions))))
    (if candidates
        (goto-char (car (last candidates)))
      (user-error "No earlier Discourse topic"))))

(defun discourse-topic-list-open-topic ()
  "Open the Discourse topic at point."
  (interactive)
  (let* ((state (discourse-topic-list--state))
         (topic-id
          (or (get-text-property
               (point) discourse-topic-list-id-property)
              (get-text-property
               (line-beginning-position) discourse-topic-list-id-property)
              (user-error "No Discourse topic at point"))))
    (discourse-topic-open
     (discourse-topic-list-state-account state) topic-id t)))

(defvar-keymap discourse-topic-list-mode-map
  :parent special-mode-map
  "g" #'discourse-topic-list-refresh
  "N" #'discourse-topic-list-load-more
  "n" #'discourse-topic-list-next
  "p" #'discourse-topic-list-previous
  "RET" #'discourse-topic-list-open-topic
  "?" #'discourse-topic-list-transient
  "q" #'quit-window)

(define-derived-mode discourse-topic-list-mode special-mode "Discourse-Topics"
  "Major mode for a projected Discourse topic list."
  (setq-local truncate-lines t))

(defun discourse-topic-list--setup (view)
  "Initialize newly attached topic-list VIEW."
  (discourse-topic-list--state view)
  (appkit-projection-ensure
   view
   :printer #'discourse-topic-list--print-row
   :anchor-property discourse-topic-list-id-property
   :no-separator-p t)
  (appkit-view-enable-responsive-geometry view)
  (appkit-view-enqueue-event view (list :position 'first))
  (appkit-invalidate view :structure t :parts '(frame entries geometry))
  (appkit-sync-invalidations view)
  (discourse-topic-list--request view 'initial))

(defun discourse-topic-list-open-latest (account &optional select)
  "Open ACCOUNT's Latest topic list and optionally SELECT it."
  (unless (and (discourse-account-p account)
               (appkit-app-live-p (discourse-account-app account)))
    (user-error "Discourse account is not running"))
  (let* ((app (discourse-account-app account))
         (view-id '(topics latest))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode #'discourse-topic-list-mode
           :buffer-name
           (format "*Discourse Latest: %s*"
                   (discourse-account-origin account))
           :state
           (discourse-topic-list-state-create
            :account account
            :topics nil
            :phase 'initial
            :loaded-p nil
            :exhausted-p nil)
           :sync-function #'discourse-topic-list--sync
           :parts '(frame entries geometry)
           :position-policy 'semantic
           :setup #'discourse-topic-list--setup
           :select select)))
    (appkit-view-buffer view)))

(provide 'discourse-topic-list)

;;; discourse-topic-list.el ends here
