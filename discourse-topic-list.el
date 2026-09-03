;;; discourse-topic-list.el --- Discourse topic list views -*- lexical-binding: t; -*-

;;; Commentary:

;; Stable-key Appkit projection for authenticated or anonymous Latest pages
;; and the server-owned `more_topics_url' cursor.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'appkit-chat-avatar)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'appkit-scroll)
(require 'appkit-ui)
(require 'appkit-view)
(require 'discourse-api)
(require 'discourse-compose)
(require 'discourse-customize)
(require 'discourse-media)
(require 'discourse-runtime)
(require 'discourse-state)
(require 'discourse-site)
(require 'discourse-ui)

(declare-function discourse-topic-open
                  "discourse-topic" (account topic-id &optional select post-number))
(declare-function discourse-topic-list-transient
                  "discourse-transient" ())

(defconst discourse-topic-list--request-key 'topics
  "View request-table key for the active topic page request.")

(defconst discourse-topic-list-poster-property
  'discourse-topic-list-poster
  "Text property carrying one featured-poster presentation record.")


(defvar-local discourse-topic-list--scroll-observer nil
  "Lifecycle-owned automatic pagination observer for this topic list.")

(defface discourse-topic-list-title
  '((t :inherit default :weight semi-bold))
  "Face for topic titles in a Discourse list."
  :group 'discourse)

(defface discourse-topic-list-title-unseen
  '((t :inherit discourse-topic-list-title :weight bold))
  "Face for unseen topic titles in a Discourse list."
  :group 'discourse)


(defconst discourse-topic-list-id-property 'discourse-topic-id
  "Text property carrying a stable Discourse topic ID.")

(cl-defstruct (discourse-topic-list-state
               (:constructor discourse-topic-list-state-create)
               (:copier nil))
  account
  topics
  more-url
  can-create-topic-p
  phase
  message
  loaded-p
  exhausted-p
  retry-phase
  retry-endpoint)

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

(defun discourse-topic-list--number (value)
  "Return compact human-readable text for numeric VALUE."
  (if (not (and (numberp value) (>= value 0)))
      "0"
    (let ((number (float value)))
      (cond
       ((< number 1000) (format "%d" (truncate number)))
       ((< number 1000000)
        (concat
         (string-remove-suffix ".0" (format "%.1f" (/ number 1000.0)))
         "k"))
       (t
        (concat
         (string-remove-suffix ".0" (format "%.1f" (/ number 1000000.0)))
         "m"))))))

(defun discourse-topic-list--format-activity (topic &optional now)
  "Return Discourse-style relative activity time for TOPIC.
NOW defaults to `current-time' and exists for deterministic callers."
  (let ((value (or (discourse-topic-list--field topic "bumped_at")
                   (discourse-topic-list--field topic "last_posted_at")
                   (discourse-topic-list--field topic "created_at"))))
    (if (not (stringp value))
        ""
      (condition-case nil
          (let ((seconds
                 (max 0
                      (float-time
                       (time-subtract (or now (current-time))
                                      (date-to-time value))))))
            (cond
             ((< seconds 60) "now")
             ((< seconds 3600) (format "%dm" (floor (/ seconds 60))))
             ((< seconds 86400) (format "%dh" (floor (/ seconds 3600))))
             ((< seconds 604800) (format "%dd" (floor (/ seconds 86400))))
             ((< seconds 2592000) (format "%dw" (floor (/ seconds 604800))))
             ((< seconds 31536000)
              (format "%dmo" (floor (/ seconds 2592000))))
             (t (format "%dy" (floor (/ seconds 31536000))))))
        (error "")))))


(defun discourse-topic-list--domain-state (list-state)
  "Return canonical domain state owned by LIST-STATE."
  (discourse-account-state
   (discourse-topic-list-state-account list-state)))


(defun discourse-topic-list--poster-name (topic domain-state)
  "Return TOPIC's latest poster name from DOMAIN-STATE."
  (let ((direct
         (discourse-topic-list--field topic "last_poster_username")))
    (if (and (stringp direct) (not (string-empty-p direct)))
        (concat "@" (substring-no-properties direct))
      (let* ((posters
              (condition-case nil
                  (discourse-state-sequence-list
                   (discourse-topic-list--field topic "posters"))
                (error nil)))
             (latest
              (seq-find
               (lambda (poster)
                 (and (hash-table-p poster)
                      (let ((extras (gethash "extras" poster)))
                        (and (stringp extras)
                             (string-match-p "\\blatest\\b" extras)))))
               posters))
             (poster (or latest (car (last posters))))
             (user
              (and (hash-table-p poster)
                   (condition-case nil
                       (discourse-state-user
                        domain-state (gethash "user_id" poster))
                     (error nil))))
             (username (and user (gethash "username" user))))
        (if (and (stringp username) (not (string-empty-p username)))
            (concat "@" (substring-no-properties username))
          "")))))

(defun discourse-topic-list--posters (topic)
  "Return TOPIC's validated featured-poster sequence."
  (cl-remove-if-not
   #'hash-table-p
   (condition-case nil
       (discourse-state-sequence-list
        (discourse-topic-list--field topic "posters"))
     (error nil))))

(defun discourse-topic-list--poster-record
    (poster account domain-state)
  "Return POSTER presentation data for ACCOUNT and DOMAIN-STATE."
  (when-let* ((raw-id (gethash "user_id" poster))
              (user-id (condition-case nil
                           (discourse-state-id raw-id)
                         (error nil)))
              (user (discourse-state-user domain-state user-id)))
    (let* ((username
            (discourse-topic-list--string
             (gethash "username" user) user-id))
           (description
            (discourse-topic-list--string
             (gethash "description" poster) "featured poster"))
           (image (discourse-media-avatar-image account user-id))
           (resized
            (and image
                 (appkit-chat-avatar-resize-image
                  image (appkit-chat-avatar-line-pixel-height)))))
      (list :user-id user-id
            :username username
            :description description
            :image resized))))

(defun discourse-topic-list--poster-token (record)
  "Return one stable text token for featured-poster RECORD."
  (let* ((username (plist-get record :username))
         (fallback (concat "@" username))
         (image (plist-get record :image))
         (text
          (if image
              (propertize fallback
                          'display image
                          'rear-nonsticky '(display))
            fallback)))
    (add-text-properties
     0 (length text)
     (list discourse-topic-list-poster-property record)
     text)
    text))

(defun discourse-topic-list--poster-strip
    (topic account domain-state)
  "Return TOPIC's ordered featured-poster strip.
The server orders the original poster first, selected frequent/recent
posters in the middle, and the latest poster last unless the original
poster is also latest."
  (string-join
   (delq nil
         (mapcar
          (lambda (poster)
            (when-let* ((record
                         (discourse-topic-list--poster-record
                          poster account domain-state)))
              (discourse-topic-list--poster-token record)))
          (discourse-topic-list--posters topic)))
   " "))

(defun discourse-topic-list--restore-poster-help (start end)
  "Restore semantic featured-poster help between START and END."
  (let ((position start))
    (while (< position end)
      (let* ((record
              (get-text-property
               position discourse-topic-list-poster-property))
             (next
              (next-single-property-change
               position discourse-topic-list-poster-property nil end)))
        (when record
          (add-text-properties
           position next
           (list
            'help-echo
            (format "@%s — %s"
                    (plist-get record :username)
                    (plist-get record :description)))))
        (setq position next)))))

(defun discourse-topic-list--topic-dependencies (topic)
  "Return presentation resources used by TOPIC."
  (delq nil
        (mapcar
         (lambda (poster)
           (when-let* ((raw-id (gethash "user_id" poster))
                       (user-id
                        (condition-case nil
                            (discourse-state-id raw-id)
                          (error nil))))
             (list :user user-id)))
         (discourse-topic-list--posters topic))))

(defun discourse-topic-list--reply-count (topic)
  "Return the Web-compatible reply count for TOPIC."
  (let ((posts (discourse-topic-list--field topic "posts_count" 0)))
    (if (and (integerp posts) (> posts 0)) (1- posts) 0)))

(defun discourse-topic-list--metadata-lines
    (topic account domain-state activity)
  "Return TOPIC's stable taxonomy, activity, and engagement lines.
DOMAIN-STATE resolves category and poster identity.  ACTIVITY is a
preformatted relative timestamp."
  (let* ((replies (discourse-topic-list--reply-count topic))
         (views (discourse-topic-list--field topic "views" 0))
         (poster (discourse-topic-list--poster-name topic domain-state))
         (tags (discourse-ui-topic-tags topic))
         (poster-strip
          (discourse-topic-list--poster-strip
           topic account domain-state))
         (taxonomy
          (concat
           (discourse-ui-topic-category-text topic domain-state)
           (unless (string-empty-p tags)
             (concat "  ·  " tags))))
         (latest
          (cond
           ((and (not (string-empty-p activity))
                 (not (string-empty-p poster)))
            (format "%s by %s" activity poster))
           ((not (string-empty-p activity)) activity)
           ((not (string-empty-p poster)) poster)
           (t "activity unavailable")))
         (engagement
          (format "%s %s  ·  %s %s"
                  (discourse-topic-list--number replies)
                  (if (= replies 1) "reply" "replies")
                  (discourse-topic-list--number views)
                  (if (and (numberp views) (= views 1))
                      "view"
                    "views"))))
    (list taxonomy
          (propertize
           (concat
            latest
            (unless (string-empty-p poster-strip)
              (concat "  ·  posters " poster-strip)))
           'face 'shadow)
          (propertize engagement 'face 'shadow))))

(defun discourse-topic-list--print-row (row)
  "Insert projected topic ROW using a stable four-line hierarchy."
  (let* ((topic (appkit-projection-row-payload row))
         (list-state (discourse-topic-list--state))
         (domain-state (discourse-topic-list--domain-state list-state))
         (account (discourse-topic-list-state-account list-state))
         (id (discourse-topic-list--id topic))
         (title
          (discourse-topic-list--string
           (discourse-topic-list--field topic "title") "(untitled topic)"))
         (unseen (discourse-ui-topic-unseen-p topic))
         (activity (discourse-topic-list--format-activity topic))
         (start (point)))
    (insert
     (discourse-ui-topic-status-text topic t)
     (propertize
      title 'face
      (if unseen
          'discourse-topic-list-title-unseen
        'discourse-topic-list-title))
     "\n")
    (dolist (line
             (discourse-topic-list--metadata-lines
              topic account domain-state activity))
      (insert "  " line "\n"))
    (add-text-properties
     start (point)
     (list discourse-topic-list-id-property id
           'discourse-topic topic))
    (appkit-ui-add-action
     start (point)
     (lambda () (discourse-topic-open account id t))
     :help-echo
     (format "Open %s — %s replies, %s views"
             title
             (discourse-topic-list--number
              (discourse-topic-list--reply-count topic))
             (discourse-topic-list--number
              (discourse-topic-list--field topic "views" 0))))
    (discourse-topic-list--restore-poster-help start (point))))

(defun discourse-topic-list--project (state)
  "Project topic-list STATE into stable rows."
  (appkit-projection-project
   (discourse-topic-list-state-topics state)
   #'discourse-topic-list--id
   :dependencies-function #'discourse-topic-list--topic-dependencies))

(defun discourse-topic-list--header-line ()
  "Return the persistent header line for the current Latest view."
  (condition-case nil
      (let* ((state (discourse-topic-list--state))
             (domain-state (discourse-topic-list--domain-state state))
             (profile (discourse-state-site-profile domain-state))
             (title
              (discourse-topic-list--string
               (and profile (gethash "title" profile))
               (discourse-account-origin
                (discourse-topic-list-state-account state))))
             (count (length (discourse-topic-list-state-topics state)))
             (status
              (pcase (discourse-topic-list-state-phase state)
                ('initial "loading")
                ('refresh "refreshing")
                ('older (format "loading · %d topics" count))
                ('error "error · R retry")
                (_
                 (format "%d topic%s%s"
                         count
                         (if (= count 1) "" "s")
                         (if (discourse-topic-list-state-exhausted-p state)
                             " · end"
                           ""))))))
        (concat
         " "
         (propertize title 'face 'bold)
         (propertize
          (format " · Latest · %s · %s · ? actions "
                  (discourse-account-display-identity
                   (discourse-topic-list-state-account state))
                  status)
          'face 'shadow)))
    (error " Discourse · Latest ")))

(defun discourse-topic-list--footer (state)
  "Return generated footer for topic-list STATE."
  (let ((count (length (discourse-topic-list-state-topics state))))
    (concat
     "\n"
     (pcase (discourse-topic-list-state-phase state)
       ('initial "Loading topics…")
       ('refresh (format "Refreshing topics… · %d retained" count))
       ('older (format "Loading more topics… · %d loaded" count))
       ('error
        (propertize
         (format "%s · R retry · %s"
                 (if (eq (discourse-topic-list-state-retry-phase state)
                         'older)
                     "Unable to load more topics"
                   "Unable to load topics")
                 (or (discourse-topic-list-state-message state)
                     "unknown error"))
         'face 'error))
       (_
        (propertize
         (cond
          ((= count 0) "No topics in Latest · g refresh")
          ((discourse-topic-list-state-exhausted-p state)
           (format "End of list · %d topic%s"
                   count (if (= count 1) "" "s")))
          (t (format "%d topics loaded · scroll for more" count)))
         'face 'shadow)))
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
         (metadata-p (memq 'metadata parts))
         (resources (appkit-invalidations-resource-keys invalidations))
         (reconcile-p
          (or metadata-p
              (appkit-invalidations-structure-p invalidations)
              (memq 'entries parts)
              entry-keys resources))
         (rows (and reconcile-p (discourse-topic-list--project state))))
    (appkit-projection-sync
     view rows
     :header ""
     :footer (discourse-topic-list--footer state)
     :force-keys (if metadata-p
                     (mapcar #'appkit-projection-row-key rows)
                   entry-keys)
     :changed-dependencies resources
     :position (discourse-topic-list--position-intent events)
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)
    (force-mode-line-update t)
    (when (and (discourse-topic-list-state-loaded-p state)
               (appkit-scroll-observer-p
                discourse-topic-list--scroll-observer))
      (appkit-scroll-observer-check
       discourse-topic-list--scroll-observer))))


(defun discourse-topic-list--failure-message (result)
  "Return presentation message for failed HTTP RESULT."
  (let ((failure (discourse-http-result-failure result)))
    (if (discourse-http-failure-p failure)
        (discourse-http-failure-message failure)
      "Unknown Discourse response failure")))

(defun discourse-topic-list--handle-error
    (view state phase endpoint result)
  "Install failed RESULT for PHASE and ENDPOINT in VIEW STATE."
  (setf (discourse-topic-list-state-phase state) 'error
        (discourse-topic-list-state-message state)
        (discourse-topic-list--failure-message result)
        (discourse-topic-list-state-retry-phase state) phase
        (discourse-topic-list-state-retry-endpoint state) endpoint)
  (appkit-request-sync view :part 'frame :position t)
  (unless (eq phase 'older)
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
    (view state phase endpoint page)
  "Install validated PAGE into VIEW STATE for PHASE.
ENDPOINT is retained only if response validation fails."
  (condition-case error-data
      (let* ((topics (discourse-topic-page-topics page))
             (users (discourse-topic-page-users page))
             (domain-state
              (discourse-account-state
               (discourse-topic-list-state-account state)))
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
        (dolist (user users)
          (discourse-state-merge-user domain-state user))
        (dolist (topic topics)
          (discourse-state-merge-topic domain-state topic))
        (discourse-state-set-can-create-topic
         domain-state
         (discourse-topic-page-can-create-topic-p page))
        (setf (discourse-topic-list-state-topics state) installed
              (discourse-topic-list-state-more-url state)
              (discourse-topic-page-more-url page)
              (discourse-topic-list-state-can-create-topic-p state)
              (discourse-topic-page-can-create-topic-p page)
              (discourse-topic-list-state-phase state) 'ready
              (discourse-topic-list-state-message state) nil
              (discourse-topic-list-state-retry-phase state) nil
              (discourse-topic-list-state-retry-endpoint state) nil
              (discourse-topic-list-state-loaded-p state) t
              (discourse-topic-list-state-exhausted-p state)
              (null (discourse-topic-page-more-url page)))
        (appkit-view-enqueue-event
         view (list :position (if (eq phase 'initial) 'first 'preserve)))
        (appkit-request-sync view :structure t :part 'frame :position t)
        (unless (eq phase 'older)
          (message "Loaded %d Discourse topics" (length topics))))
    (error
     (let ((result
            (discourse-http-result-create
             :ok-p nil
             :failure
             (discourse-http-failure-create
              :kind 'invalid-response
              :message (error-message-string error-data)))))
       (discourse-topic-list--handle-error
        view state phase endpoint result)))))

(defun discourse-topic-list--update-buffer-name (view profile)
  "Rename VIEW from validated site PROFILE."
  (when (and (appkit-view-live-p view) (hash-table-p profile))
    (let ((title (gethash "title" profile)))
      (when (and (stringp title) (not (string-empty-p title)))
        (with-current-buffer (appkit-view-buffer view)
          (rename-buffer (format "*Discourse: %s · Latest*" title) t))))))

(defun discourse-topic-list--request-site-metadata (view)
  "Ensure shared public site metadata needed by VIEW."
  (let* ((state (discourse-topic-list--state view))
         (account (discourse-topic-list-state-account state))
         (domain-state (discourse-account-state account)))
    (when-let* ((profile (discourse-state-site-profile domain-state)))
      (discourse-topic-list--update-buffer-name view profile))
    (discourse-site-ensure-metadata
     account
     (lambda (kind)
       (when (and (appkit-view-live-p view)
                  (eq state (appkit-view-state view)))
         (pcase kind
           ('categories
            (appkit-request-sync view :part 'metadata))
           ('profile
            (discourse-topic-list--update-buffer-name
             view (discourse-state-site-profile domain-state))
            (appkit-request-sync view :part 'frame)))))
     :owner view)))


(defun discourse-topic-list--request (view phase &optional retry-endpoint)
  "Start topic-list VIEW request for PHASE.
RETRY-ENDPOINT, when non-nil, is the exact failed endpoint to replay."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Discourse topic request phase"))
  (let* ((state (discourse-topic-list--state view))
         (endpoint
          (or retry-endpoint
              (if (eq phase 'older)
                  (or (discourse-topic-list-state-more-url state)
                      (user-error "No more Discourse topics"))
                "/latest.json"))))
    (when (and (eq phase 'older)
               (discourse-topic-list-state-exhausted-p state))
      (user-error "No more Discourse topics"))
    (let ((operation
           (appkit-view-operation-begin
            view discourse-topic-list--request-key)))
      (setf (discourse-topic-list-state-phase state) phase
            (discourse-topic-list-state-message state) nil
            (discourse-topic-list-state-retry-phase state) nil
            (discourse-topic-list-state-retry-endpoint state) nil)
      (appkit-request-sync view :part 'frame :position t)
      (discourse-api-topic-page
       (discourse-topic-list-state-account state)
       (lambda (result)
         (when (appkit-view-operation-finish operation)
           (if (discourse-http-result-ok-p result)
               (discourse-topic-list--handle-success
                view state phase endpoint
                (discourse-http-result-data result))
             (discourse-topic-list--handle-error
              view state phase endpoint result))))
       :endpoint endpoint
       :owner operation))))

(defun discourse-topic-list-refresh ()
  "Refresh the current Discourse topic list."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic list")))
         (state (discourse-topic-list--state view)))
    (discourse-topic-list--request
     view
     (if (discourse-topic-list-state-loaded-p state) 'refresh 'initial))))

(defun discourse-topic-list-retry-available-p ()
  "Return non-nil when the current topic list has a failed request."
  (condition-case nil
      (let ((state (discourse-topic-list--state)))
        (and (eq (discourse-topic-list-state-phase state) 'error)
             (discourse-topic-list-state-retry-phase state)
             (discourse-topic-list-state-retry-endpoint state)))
    (error nil)))

(defun discourse-topic-list-retry ()
  "Retry the current topic list's exact failed request."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic list")))
         (state (discourse-topic-list--state view))
         (phase (discourse-topic-list-state-retry-phase state))
         (endpoint (discourse-topic-list-state-retry-endpoint state)))
    (unless (and (eq (discourse-topic-list-state-phase state) 'error)
                 phase endpoint)
      (user-error "No failed Discourse topic request to retry"))
    (discourse-topic-list--request view phase endpoint)))


(defun discourse-topic-list--maybe-auto-load
    (view _window position end)
  "Load VIEW's next topic page when POSITION approaches END."
  (when (and (appkit-view-live-p view)
             (numberp discourse-scroll-load-threshold)
             (appkit-scroll-near-end-p
              position end discourse-scroll-load-threshold))
    (let ((state (discourse-topic-list--state view)))
      (when (and (discourse-topic-list-state-loaded-p state)
                 (eq (discourse-topic-list-state-phase state) 'ready)
                 (discourse-topic-list-state-more-url state)
                 (not (discourse-topic-list-state-exhausted-p state)))
        (discourse-topic-list--request view 'older)))))

(defun discourse-topic-list--install-scroll-observer (view)
  "Install VIEW's lifecycle-owned topic pagination observer."
  (setq-local
   discourse-topic-list--scroll-observer
   (appkit-scroll-observer-install
    view
    :end-function
    (lambda (window position end)
      (discourse-topic-list--maybe-auto-load
       view window position end)))))

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

(defun discourse-topic-list-can-create-topic-p ()
  "Return non-nil when the server permits creating a topic here."
  (condition-case nil
      (let ((state (discourse-topic-list--state)))
        (and (discourse-topic-list-state-loaded-p state)
             (discourse-topic-list-state-can-create-topic-p state)
             (discourse-compose-new-topic-allowed-p
              (discourse-topic-list-state-account state))))
    (error nil)))

(defun discourse-topic-list-compose-topic ()
  "Open a new-topic composer for the current authenticated account."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "No live Discourse topic list")))
         (state (discourse-topic-list--state view)))
    (unless (discourse-topic-list-can-create-topic-p)
      (user-error "The server does not currently allow topic creation"))
    (discourse-compose-new-topic
     (discourse-topic-list-state-account state)
     :source-view view
     :select t)))

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
  "c" #'discourse-topic-list-compose-topic
  "n" #'discourse-topic-list-next
  "R" #'discourse-topic-list-retry
  "p" #'discourse-topic-list-previous
  "RET" #'discourse-topic-list-open-topic
  "?" #'discourse-topic-list-transient
  "q" #'quit-window)

(define-derived-mode discourse-topic-list-mode special-mode "Discourse-Topics"
  "Major mode for a projected Discourse topic list."
  (setq-local truncate-lines t
              header-line-format
              '(:eval (discourse-topic-list--header-line))))

(defun discourse-topic-list--setup (view)
  "Initialize newly attached topic-list VIEW."
  (discourse-topic-list--state view)
  (appkit-projection-ensure
   view
   :printer #'discourse-topic-list--print-row
   :anchor-property discourse-topic-list-id-property
   :no-separator-p t)
  (discourse-topic-list--install-scroll-observer view)
  (appkit-view-enqueue-event view (list :position 'first))
  (appkit-invalidate view :structure t :parts '(frame entries))
  (appkit-sync-invalidations view)
  (discourse-topic-list--request-site-metadata view)
  (discourse-topic-list--request view 'initial))

(defun discourse-topic-list-open-latest (account &optional select)
  "Open ACCOUNT's Latest topic list and optionally SELECT it."
  (unless (and (discourse-account-p account)
               (appkit-app-live-p (discourse-account-app account)))
    (user-error "Discourse account is not running"))
  (let* ((app (discourse-account-app account))
         (view-id '(topics latest))
         (existing (appkit-view-for-id app view-id))
         (state
          (if existing
              (appkit-view-state existing)
            (discourse-topic-list-state-create
             :account account
             :topics nil
             :phase 'initial
             :loaded-p nil
             :exhausted-p nil)))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode #'discourse-topic-list-mode
           :buffer-name
           (format "*Discourse: %s · Latest*"
                   (discourse-account-origin account))
           :state state
           :sync-function #'discourse-topic-list--sync
           :parts '(frame entries metadata)
           :position-policy 'semantic
           :setup #'discourse-topic-list--setup
           :select select)))
    (appkit-view-buffer view)))

(provide 'discourse-topic-list)

;;; discourse-topic-list.el ends here
