;;; discourse-compose.el --- Native Discourse topic and reply composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A client-owned Markdown editing surface backed by Appkit compose generation,
;; immutable capture, effect ownership, and cancellation.  Discourse remains
;; authoritative for permissions, validation, accepted posts, and queueing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-compose)
(require 'appkit-core)
(require 'appkit-markup)
(require 'appkit-markup-codecs)
(require 'appkit-markup-ui)
(require 'discourse-api)
(require 'discourse-runtime)
(require 'discourse-state)

(declare-function discourse-topic-open
                  "discourse-topic" (account topic-id &optional select post-number))
(declare-function discourse-topic-refresh "discourse-topic" ())
(declare-function discourse-topic-refresh-to-post-number
                  "discourse-topic" (post-number))
(declare-function discourse-topic-list-refresh "discourse-topic-list" ())

(defvar-local discourse-compose-kind nil
  "Current composer kind, either `topic' or `reply'.")

(defvar-local discourse-compose-account nil
  "Authenticated account owning the current composer.")

(defvar-local discourse-compose-source-view nil
  "Generated Surface from which the current composer was opened.")

(defvar-local discourse-compose-topic-id nil
  "Reply topic ID, or nil for a new topic.")

(defvar-local discourse-compose-reply-to-post-number nil
  "Topic-local post number targeted by this reply, or nil.")

(defvar-local discourse-compose-reply-to-username nil
  "Visible username targeted by this reply, or nil.")

(defvar-local discourse-compose-title nil
  "New-topic title owned by this composer.")

(defvar-local discourse-compose-category-id nil
  "New-topic category ID owned by this composer.")

(defvar-local discourse-compose-tags nil
  "Ordered new-topic tag names owned by this composer.")

(defvar-local discourse-compose-opened-at nil
  "Wall-clock time at which this composer opened.")

(defvar-local discourse-compose-typing-duration 0
  "Approximate active typing duration in milliseconds.")

(defvar-local discourse-compose--last-typing-at nil
  "Last wall-clock time credited to active typing.")

(defvar-local discourse-compose-write-outcome nil
  "Non-nil when the previous write outcome is uncertain.")

(defvar-local discourse-compose-message nil
  "Last server or transport status for this composer.")

(defvar-local discourse-compose--accepted-p nil
  "Non-nil while closing after a server-accepted write.")

(defvar-keymap discourse-compose-mode-map
  :parent text-mode-map
  "C-c C-c" #'discourse-compose-submit
  "C-c C-k" #'discourse-compose-cancel
  "C-c C-p" #'discourse-compose-preview
  "C-c C-t" #'discourse-compose-set-title
  "C-c C-g" #'discourse-compose-set-category
  "C-c C-a" #'discourse-compose-set-tags)

(defvar discourse-compose--generated-edit-p nil
  "Dynamically non-nil while generated text must not count as typing.")

(define-derived-mode discourse-compose-mode text-mode "Discourse-Compose"
  "Major mode for composing a Discourse topic or reply as raw Markdown."
  (setq-local header-line-format '(:eval (discourse-compose--header-line))
              require-final-newline nil)
  (visual-line-mode 1))

(defun discourse-compose--state ()
  "Return canonical state for the current composer."
  (unless (and (discourse-account-p discourse-compose-account)
               (appkit-app-live-p
                (discourse-account-app discourse-compose-account)))
    (error "Current buffer has no live Discourse compose account"))
  (discourse-account-state discourse-compose-account))

(defun discourse-compose-new-topic-allowed-p (account)
  "Return non-nil when ACCOUNT may create a new topic."
  (and (discourse-account-authenticated-p account)
       (let ((state (discourse-account-state account)))
         (and (discourse-state-can-create-topic-known-p state)
              (discourse-state-can-create-topic-p state)))))

(defun discourse-compose-reply-allowed-p (account topic-id)
  "Return non-nil when ACCOUNT may reply to TOPIC-ID."
  (and (discourse-account-authenticated-p account)
       (when-let* ((topic
                    (discourse-state-topic
                     (discourse-account-state account) topic-id))
                   (details (gethash "details" topic)))
         (and (hash-table-p details)
              (eq t (gethash "can_create_post" details))))))

(defun discourse-compose--category-permitted-p (category)
  "Return non-nil when CATEGORY grants full topic-creation permission."
  (and (hash-table-p category)
       (equal 1 (gethash "permission" category))))

(defun discourse-compose--permitted-categories ()
  "Return permitted canonical categories for the current composer."
  (let (categories)
    (maphash
     (lambda (_id category)
       (when (discourse-compose--category-permitted-p category)
         (push category categories)))
     (discourse-state-categories (discourse-compose--state)))
    (sort categories
          (lambda (left right)
            (string-lessp
             (or (gethash "name" left) "")
             (or (gethash "name" right) ""))))))

(defun discourse-compose--category (category-id)
  "Return canonical CATEGORY-ID for the current composer, or nil."
  (and category-id
       (condition-case nil
           (discourse-state-category
            (discourse-compose--state) category-id)
         (error nil))))

(defun discourse-compose--category-label (category)
  "Return a unique completion label for CATEGORY."
  (format "%s  [#%s]"
          (or (gethash "name" category) "Unnamed category")
          (discourse-state-id (gethash "id" category))))

(defun discourse-compose--ensure-idle ()
  "Reject metadata edits while one compose operation is active."
  (when (appkit-compose-operation-active-p)
    (user-error "A Discourse compose operation is already in progress")))

(defun discourse-compose--source ()
  "Return an owned raw Markdown snapshot of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun discourse-compose--elapsed-milliseconds ()
  "Return nonnegative elapsed composer-open time in milliseconds."
  (max 0 (round (* 1000 (- (float-time)
                           (or discourse-compose-opened-at (float-time)))))))

(defun discourse-compose--snapshot ()
  "Return an immutable client-owned Discourse draft."
  (list :kind discourse-compose-kind
        :raw (discourse-compose--source)
        :title (and discourse-compose-title
                    (substring-no-properties discourse-compose-title))
        :category-id discourse-compose-category-id
        :tags (copy-sequence discourse-compose-tags)
        :topic-id discourse-compose-topic-id
        :reply-to-post-number discourse-compose-reply-to-post-number
        :composer-open-duration (discourse-compose--elapsed-milliseconds)
        :typing-duration discourse-compose-typing-duration))

(defun discourse-compose--track-typing (_beg _end _old-length)
  "Credit one throttled semantic source edit to active typing time."
  (unless discourse-compose--generated-edit-p
    (let ((now (float-time)))
      (when (or (null discourse-compose--last-typing-at)
                (>= (- now discourse-compose--last-typing-at) 0.1))
        (setq discourse-compose-typing-duration
              (+ discourse-compose-typing-duration 100)
              discourse-compose--last-typing-at now)))))

(defun discourse-compose--state-changed (_session)
  "Refresh generated composer presentation after an Appkit state change."
  (when (and discourse-compose-message
             (not discourse-compose-write-outcome)
             (not (appkit-compose-operation-active-p)))
    (setq-local discourse-compose-message nil))
  (force-mode-line-update t))

(defun discourse-compose--status-fields ()
  "Return Appkit status fields for the current composer."
  (let ((fields
         (list
          (list :label "Identity"
                :value (format "@%s"
                               (discourse-account-display-name
                                discourse-compose-account)))
          (list :label "Type"
                :value (if (eq discourse-compose-kind 'topic)
                           "New topic"
                         "Reply")))))
    (if (eq discourse-compose-kind 'topic)
        (setq fields
              (append
               fields
               (list
                (list :label "Title"
                      :value (or discourse-compose-title "unset")
                      :action #'discourse-compose-set-title
                      :help-echo "Set the topic title")
                (list :label "Category"
                      :value
                      (if-let* ((category
                                 (discourse-compose--category
                                  discourse-compose-category-id)))
                          (or (gethash "name" category) "unnamed")
                        "unset")
                      :action #'discourse-compose-set-category
                      :help-echo "Select a permitted category")
                (list :label "Tags"
                      :value
                      (if discourse-compose-tags
                          (string-join discourse-compose-tags ",")
                        "none")
                      :action #'discourse-compose-set-tags
                      :help-echo "Edit topic tags"))))
      (setq fields
            (append
             fields
             (list
              (list :label "Topic" :value discourse-compose-topic-id)
              (when discourse-compose-reply-to-post-number
                (list
                 :label "Target"
                 :value
                 (format "#%d%s"
                         discourse-compose-reply-to-post-number
                         (if discourse-compose-reply-to-username
                             (format " @%s" discourse-compose-reply-to-username)
                           ""))))))))
    (setq fields (delq nil fields))
    (cond
     ((appkit-compose-status-text)
      (setq fields
            (append fields
                    (list (list :label "State"
                                :value (appkit-compose-status-text))))))
     (discourse-compose-write-outcome
      (setq fields
            (append fields
                    (list (list :label "State"
                                :value "outcome unknown"
                                :face 'warning)))))
     (discourse-compose-message
      (setq fields
            (append fields
                    (list (list :label "State"
                                :value discourse-compose-message
                                :face 'error))))))
    fields))

(defun discourse-compose--header-line ()
  "Return generated header-line text for the current composer."
  (condition-case nil
      (concat
       " "
       (appkit-compose-status-fields-string
        (discourse-compose--status-fields))
       (propertize "   C-c C-c send · C-c C-p preview · C-c C-k cancel "
                   'face 'shadow))
    (error " Discourse Compose ")))

(defun discourse-compose-set-title (&optional title)
  "Set new-topic TITLE and advance the semantic generation."
  (interactive)
  (unless (eq discourse-compose-kind 'topic)
    (user-error "Replies do not have a title"))
  (discourse-compose--ensure-idle)
  (setq title
        (string-trim
         (or title
             (read-string "Topic title: " discourse-compose-title))))
  (when (string-empty-p title)
    (user-error "Topic title must not be empty"))
  (unless (equal title discourse-compose-title)
    (setq-local discourse-compose-title (substring-no-properties title))
    (appkit-compose-touch))
  discourse-compose-title)

(defun discourse-compose--install-category-template (category)
  "Insert CATEGORY's server-supplied template into an empty body."
  (let ((template (and (hash-table-p category)
                       (gethash "topic_template" category))))
    (when (and (stringp template)
               (not (string-empty-p template))
               (string-empty-p (string-trim (discourse-compose--source))))
      (let ((discourse-compose--generated-edit-p t))
        (appkit-compose-without-tracking
          (erase-buffer)
          (insert (substring-no-properties template)))))))

(defun discourse-compose-set-category (&optional category-id)
  "Set new-topic CATEGORY-ID from server-permitted categories."
  (interactive)
  (unless (eq discourse-compose-kind 'topic)
    (user-error "Replies inherit their topic category"))
  (discourse-compose--ensure-idle)
  (let* ((categories (discourse-compose--permitted-categories))
         (choices
          (mapcar
           (lambda (category)
             (cons (discourse-compose--category-label category) category))
           categories)))
    (unless choices
      (user-error "The server exposes no category where this account may create topics"))
    (let* ((category
            (if category-id
                (or (cl-find-if
                     (lambda (candidate)
                       (equal (discourse-state-id (gethash "id" candidate))
                              (discourse-state-id category-id)))
                     categories)
                    (user-error "Category is not permitted for topic creation"))
              (cdr
               (assoc
                (completing-read "Topic category: " choices nil t)
                choices))))
           (selected-id (discourse-state-id (gethash "id" category))))
      (unless (equal selected-id discourse-compose-category-id)
        (setq-local discourse-compose-category-id selected-id)
        (discourse-compose--install-category-template category)
        (appkit-compose-touch))
      selected-id)))

(defun discourse-compose--observed-tag-names ()
  "Return sorted tag names observed in canonical topic state."
  (let (names)
    (maphash
     (lambda (_id topic)
       (dolist (tag
                (condition-case nil
                    (discourse-state-sequence-list (gethash "tags" topic))
                  (error nil)))
         (let ((name (cond
                      ((stringp tag) tag)
                      ((hash-table-p tag) (gethash "name" tag)))))
           (when (and (stringp name) (not (string-empty-p name)))
             (push (substring-no-properties name) names)))))
     (discourse-state-topics (discourse-compose--state)))
    (sort (delete-dups names) #'string-lessp)))

(defun discourse-compose-set-tags (&optional tags)
  "Set new-topic TAGS as an ordered list of names."
  (interactive)
  (unless (eq discourse-compose-kind 'topic)
    (user-error "Reply tags are inherited from the topic"))
  (discourse-compose--ensure-idle)
  (setq tags
        (or tags
            (completing-read-multiple
             "Topic tags: "
             (discourse-compose--observed-tag-names)
             nil nil
             (and discourse-compose-tags
                  (string-join discourse-compose-tags ",")))))
  (unless (proper-list-p tags)
    (error "Discourse compose tags must be a proper list"))
  (let ((normalized
         (delete-dups
          (delq nil
                (mapcar
                 (lambda (tag)
                   (when (stringp tag)
                     (let ((name (string-trim tag)))
                       (and (not (string-empty-p name))
                            (substring-no-properties name)))))
                 tags)))))
    (unless (equal normalized discourse-compose-tags)
      (setq-local discourse-compose-tags normalized)
      (appkit-compose-touch))
    discourse-compose-tags))

(defun discourse-compose--minimum-tags ()
  "Return current category's server-declared minimum tag count."
  (let* ((category (discourse-compose--category discourse-compose-category-id))
         (minimum (and category (gethash "minimum_required_tags" category))))
    (if (and (integerp minimum) (> minimum 0)) minimum 0)))

(defun discourse-compose--validate-draft (draft)
  "Validate DRAFT against known server capability metadata."
  (pcase (plist-get draft :kind)
    ('topic
     (unless (discourse-compose-new-topic-allowed-p discourse-compose-account)
       (user-error "The server does not currently allow this account to create topics"))
     (let* ((category-id (plist-get draft :category-id))
            (category (discourse-compose--category category-id)))
       (unless (discourse-compose--category-permitted-p category)
         (user-error "Select a category where the server permits topic creation")))
     (unless (and (stringp (plist-get draft :title))
                  (not (string-empty-p (string-trim
                                        (plist-get draft :title)))))
       (user-error "Set a topic title before sending"))
     (let ((minimum (discourse-compose--minimum-tags)))
       (when (< (length (plist-get draft :tags)) minimum)
         (user-error "This category requires at least %d tag%s"
                     minimum (if (= minimum 1) "" "s")))))
    ('reply
     (unless (discourse-compose-reply-allowed-p
              discourse-compose-account discourse-compose-topic-id)
       (user-error "The server does not currently allow replies to this topic")))
    (_ (error "Invalid Discourse compose kind")))
  (when (string-empty-p (string-trim (plist-get draft :raw)))
    (user-error "Post body must not be empty"))
  draft)

(defun discourse-compose-preview ()
  "Open a native local Markdown preview of the current immutable capture."
  (interactive)
  (let* ((capture (appkit-compose-capture))
         (draft (plist-get capture :value))
         (raw (plist-get draft :raw))
         (parsed (appkit-markup-parse 'markdown raw))
         (document (appkit-markup-parse-result-document parsed))
         (buffer (get-buffer-create "*Discourse Compose Preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize "Local Markdown preview\n\n" 'face 'bold))
        (appkit-markup-ui-insert-document
         document :final-newline-p t :interactive-p nil)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun discourse-compose--failure-message (result)
  "Return a bounded presentation message from failed HTTP RESULT."
  (let* ((failure (discourse-http-result-failure result))
         (message
          (if (discourse-http-failure-p failure)
              (discourse-http-failure-message failure)
            "Unknown Discourse write failure"))
         (retry-after
          (and (discourse-http-failure-p failure)
               (discourse-http-failure-retry-after failure))))
    (if retry-after
        (format "%s · retry after %ss" message retry-after)
      message)))

(defun discourse-compose--unknown-result-p (result)
  "Return non-nil when failed write RESULT may have reached the server."
  (let* ((failure (discourse-http-result-failure result))
         (kind (and (discourse-http-failure-p failure)
                    (discourse-http-failure-kind failure)))
         (status (or (and (discourse-http-failure-p failure)
                          (discourse-http-failure-status failure))
                     0)))
    (not (or (eq kind 'credential)
             (and (eq kind 'http) (<= 400 status 499))))))

(defun discourse-compose--operation-current-p (buffer owner)
  "Return non-nil when OWNER still owns BUFFER's compose effect."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (appkit-compose-operation-current-p owner))))

(defun discourse-compose--refresh-source (&optional post-number)
  "Refresh the live source view, optionally targeting POST-NUMBER."
  (when (appkit-surface-live-p discourse-compose-source-view)
    (let ((buffer (appkit-surface-buffer discourse-compose-source-view)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (cond
           ((and post-number
                 (derived-mode-p 'discourse-topic-mode))
            (discourse-topic-refresh-to-post-number post-number))
           ((derived-mode-p 'discourse-topic-mode)
            (discourse-topic-refresh))
           ((derived-mode-p 'discourse-topic-list-mode)
            (discourse-topic-list-refresh))))))))

(defun discourse-compose--close-accepted (buffer)
  "Close accepted compose BUFFER without a discard prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local discourse-compose--accepted-p t)
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun discourse-compose--accept-posted (buffer created)
  "Reconcile an immediately CREATED post and close BUFFER."
  (let* ((post (discourse-created-post-post created))
         (account
          (with-current-buffer buffer discourse-compose-account))
         (topic-id (discourse-state-id (gethash "topic_id" post)))
         (post-number (gethash "post_number" post))
         (source-view
          (with-current-buffer buffer discourse-compose-source-view)))
    (discourse-state-merge-post (discourse-account-state account) post)
    (if (and (appkit-surface-live-p source-view)
             (with-current-buffer (appkit-surface-buffer source-view)
               (derived-mode-p 'discourse-topic-mode)))
        (progn
          (with-current-buffer buffer
            (discourse-compose--refresh-source post-number))
          (pop-to-buffer (appkit-surface-buffer source-view)))
      (discourse-topic-open account topic-id t post-number))
    (discourse-compose--close-accepted buffer)
    (message "Discourse accepted post #%d" post-number)))

(defun discourse-compose--accept-queued (buffer created)
  "Accept queued CREATED result and close BUFFER."
  (let ((server-message (discourse-created-post-message created)))
    (with-current-buffer buffer
      (discourse-compose--refresh-source))
    (discourse-compose--close-accepted buffer)
    (message "%s" (or server-message
                      "Discourse queued the post for approval"))))

(defun discourse-compose--settle-success (buffer owner created)
  "Settle BUFFER OWNER with accepted CREATED result."
  (when (discourse-compose--operation-current-p buffer owner)
    (with-current-buffer buffer
      (appkit-compose-operation-finish owner)
      (setq-local buffer-read-only nil
                  discourse-compose-message nil
                  discourse-compose-write-outcome nil))
    (pcase (discourse-created-post-outcome created)
      ('posted (discourse-compose--accept-posted buffer created))
      ('queued (discourse-compose--accept-queued buffer created))
      (_ (error "Invalid accepted Discourse write outcome")))))

(defun discourse-compose--settle-failure (buffer owner result)
  "Settle BUFFER OWNER after failed write RESULT."
  (when (discourse-compose--operation-current-p buffer owner)
    (let* ((unknown-p (discourse-compose--unknown-result-p result))
           (failure (discourse-http-result-failure result))
           (status (and (discourse-http-failure-p failure)
                        (discourse-http-failure-status failure)))
           (refresh-p (or unknown-p (memq status '(403 404))))
           (text (discourse-compose--failure-message result)))
      (with-current-buffer buffer
        (appkit-compose-operation-finish owner)
        (setq-local buffer-read-only nil
                    discourse-compose-write-outcome
                    (and unknown-p 'unknown)
                    discourse-compose-message text)
        (force-mode-line-update t)
        (when refresh-p
          (discourse-compose--refresh-source)))
      (message "%s%s"
               text
               (if unknown-p
                   "; inspect the server before explicitly sending again"
                 "")))))

(defun discourse-compose--cancel-write (buffer owner request)
  "Cancel REQUEST for BUFFER OWNER and retain an uncertain editable draft."
  (discourse-http-cancel request)
  (when (discourse-compose--operation-current-p buffer owner)
    (discourse-compose--settle-failure
     buffer owner
     (discourse-http-result-create
      :ok-p nil
      :failure
      (discourse-http-failure-create
       :kind 'cancelled
       :status 0
       :message "Discourse send was canceled")))))

(defun discourse-compose--submit-callback (buffer owner result)
  "Settle BUFFER OWNER from API RESULT."
  (when (discourse-compose--operation-current-p buffer owner)
    (if (discourse-http-result-ok-p result)
        (discourse-compose--settle-success
         buffer owner (discourse-http-result-data result))
      (discourse-compose--settle-failure buffer owner result))))

(defun discourse-compose-submit ()
  "Submit the current immutable draft exactly once without automatic retry."
  (interactive) (discourse-compose--ensure-idle)
  (when discourse-compose-write-outcome
    (unless
        (yes-or-no-p
         "The previous send may have succeeded; send this draft again anyway? ")
      (user-error "Discourse resend canceled"))
    (setq-local discourse-compose-write-outcome nil
                discourse-compose-message nil))
  (let*
      ((capture (appkit-compose-capture))
       (generation (plist-get capture :generation))
       (draft
        (discourse-compose--validate-draft (plist-get capture :value)))
       (buffer (current-buffer))
       (view (discourse-account-app discourse-compose-account))
       (owner
        (appkit-compose-operation-begin 'submitting :generation
                                        generation :label
                                        (if
                                            (eq discourse-compose-kind
                                                'topic)
                                            "Creating topic…"
                                          "Sending reply…")))
       request callback-ran-p)
    (setq-local buffer-read-only t discourse-compose-message nil)
    (condition-case error-data
        (progn
          (setq request
                (pcase (plist-get draft :kind)
                  ('topic
                   (discourse-api-create-topic
                    discourse-compose-account (plist-get draft :title)
                    (plist-get draft :raw)
                    (lambda (result) (setq callback-ran-p t)
                      (discourse-compose--submit-callback buffer owner
                                                          result))
                    :category-id (plist-get draft :category-id) :tags
                    (plist-get draft :tags) :composer-open-duration
                    (plist-get draft :composer-open-duration)
                    :typing-duration
                    (plist-get draft :typing-duration) :owner view))
                  ('reply
                   (discourse-api-create-reply
                    discourse-compose-account
                    (plist-get draft :topic-id) (plist-get draft :raw)
                    (lambda (result) (setq callback-ran-p t)
                      (discourse-compose--submit-callback buffer owner
                                                          result))
                    :reply-to-post-number
                    (plist-get draft :reply-to-post-number)
                    :composer-open-duration
                    (plist-get draft :composer-open-duration)
                    :typing-duration
                    (plist-get draft :typing-duration) :owner view))))
          (when
              (and request (not callback-ran-p)
                   (appkit-compose-operation-current-p owner))
            (appkit-compose-operation-update owner :cancel-function
                                             (lambda ()
                                               (discourse-compose--cancel-write
                                                buffer owner request)))))
      (error
       (when (appkit-compose-operation-current-p owner)
         (appkit-compose-operation-finish owner)
         (setq-local buffer-read-only nil))
       (signal (car error-data) (cdr error-data))))))

(defun discourse-compose-cancel ()
  "Cancel an active send, or discard the current editable composer."
  (interactive)
  (if (appkit-compose-operation-active-p)
      (appkit-compose-cancel-operation)
    (when (and (buffer-modified-p)
               (not (yes-or-no-p "Discard this Discourse draft? ")))
      (user-error "Draft kept"))
    (setq-local discourse-compose--accepted-p t)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defun discourse-compose--confirm-kill ()
  "Confirm killing an unaccepted modified composer."
  (or discourse-compose--accepted-p
      (not (buffer-modified-p))
      (yes-or-no-p "Discard this Discourse draft? ")))

(defun discourse-compose--buffer-name (account kind topic-id reply-number)
  "Return content-free compose buffer name for ACCOUNT context."
  (format "*Discourse Compose: %s · @%s · %s*"
          (discourse-account-origin account)
          (discourse-account-display-name account)
          (pcase kind
            ('topic "new topic")
            ('reply
             (format "t/%s%s"
                     topic-id
                     (if reply-number
                         (format "#%d" reply-number)
                       ""))))))

(cl-defun discourse-compose--open
    (account kind &key source-view topic-id reply-to-post-number
             reply-to-username title category-id tags select)
  "Open or reuse ACCOUNT's composer for KIND and supplied context."
  (unless
      (and (discourse-account-authenticated-p account)
           (appkit-app-live-p (discourse-account-app account)))
    (user-error
     "Discourse composition requires a live User API Key account"))
  (let*
      ((app (discourse-account-app account))
       (view-id
        (pcase kind
          ('topic '(compose new-topic))
          ('reply
           (list 'compose 'reply (discourse-state-id topic-id)
                 reply-to-post-number))
          (_ (error "Invalid Discourse compose kind"))))
       (existing
        (gethash view-id (discourse-account-composers account))))
    (if (buffer-live-p existing)
        (let ((buffer existing))
          (when select (pop-to-buffer buffer)) buffer)
      (let
          ((buffer
            (generate-new-buffer
             (discourse-compose--buffer-name account kind topic-id
                                             reply-to-post-number))))
        (condition-case error-data
            (with-current-buffer buffer
              (discourse-compose-mode)
              (setq-local discourse-compose-kind kind
                          discourse-compose-account account
                          discourse-compose-source-view source-view
                          discourse-compose-topic-id
                          (and topic-id (discourse-state-id topic-id))
                          discourse-compose-reply-to-post-number
                          reply-to-post-number
                          discourse-compose-reply-to-username
                          (and reply-to-username
                               (substring-no-properties
                                reply-to-username))
                          discourse-compose-title
                          (and title (substring-no-properties title))
                          discourse-compose-category-id
                          (and category-id
                               (discourse-state-id category-id))
                          discourse-compose-tags (copy-sequence tags)
                          discourse-compose-opened-at (float-time)
                          discourse-compose-typing-duration 0
                          discourse-compose-write-outcome nil
                          discourse-compose-message nil)
              (when (and (eq kind 'topic) category-id)
                (discourse-compose--install-category-template
                 (discourse-compose--category category-id)))
              (goto-char (point-max))
              (let
                  ((handle
                    (appkit-register-handle app 'compose-buffer buffer
                                            (lambda (host)
                                              (when
                                                  (buffer-live-p host)
                                                (with-current-buffer
                                                    host
                                                  (setq-local
                                                   discourse-compose--accepted-p
                                                   t)
                                                  (set-buffer-modified-p
                                                   nil))
                                                (kill-buffer host))))))
                (puthash view-id buffer
                         (discourse-account-composers account))
                (add-hook 'kill-buffer-hook
                          (lambda ()
                            (when
                                (eq buffer
                                    (gethash view-id
                                             (discourse-account-composers
                                              account)))
                              (remhash view-id
                                       (discourse-account-composers
                                        account)))
                            (appkit-retire-handle handle))
                          nil t))
              (appkit-compose-setup :snapshot-function
                                    #'discourse-compose--snapshot
                                    :state-change-function
                                    #'discourse-compose--state-changed)
              (add-hook 'after-change-functions
                        #'discourse-compose--track-typing t t)
              (add-hook 'kill-buffer-query-functions
                        #'discourse-compose--confirm-kill nil t)
              (set-buffer-modified-p nil)
              (when select (pop-to-buffer buffer)) buffer)
          (error
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq-local discourse-compose--accepted-p t)
               (set-buffer-modified-p nil))
             (kill-buffer buffer))
           (signal (car error-data) (cdr error-data))))))))

(cl-defun discourse-compose-new-topic
    (account &key source-view title category-id tags (select t))
  "Open ACCOUNT's new-topic composer with optional initial metadata."
  (unless (discourse-compose-new-topic-allowed-p account)
    (user-error "The server does not currently allow this account to create topics"))
  (discourse-compose--open
   account 'topic
   :source-view source-view
   :title title
   :category-id category-id
   :tags tags
   :select select))

(cl-defun discourse-compose-reply
    (account topic-id &key source-view reply-to-post-number
             reply-to-username (select t))
  "Open ACCOUNT's reply composer for TOPIC-ID and optional post target."
  (unless (discourse-compose-reply-allowed-p account topic-id)
    (user-error "The server does not currently allow replies to this topic"))
  (discourse-compose--open
   account 'reply
   :source-view source-view
   :topic-id topic-id
   :reply-to-post-number reply-to-post-number
   :reply-to-username reply-to-username
   :select select))

(provide 'discourse-compose)

;;; discourse-compose.el ends here
