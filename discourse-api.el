;;; discourse-api.el --- Discourse anonymous API contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Validate anonymous read shapes and authenticated identity/write responses.
;; Views consume only normalized top-level contracts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'discourse-http)
(require 'discourse-state)

(cl-defstruct (discourse-topic-page
               (:constructor discourse-topic-page-create)
               (:copier nil))
  topics
  users
  more-url
  can-create-topic-p)

(cl-defstruct (discourse-topic-snapshot
               (:constructor discourse-topic-snapshot-create)
               (:copier nil))
  topic
  stream
  posts)

(cl-defstruct (discourse-created-post
               (:constructor discourse-created-post-create)
               (:copier nil))
  outcome
  post
  pending-post
  message)

(defun discourse-api--object (value label)
  "Return hash-table VALUE or reject it as LABEL."
  (unless (hash-table-p value)
    (error "Invalid Discourse %s object" label))
  value)

(defun discourse-api--objects (value label)
  "Return sequence VALUE as validated object list for LABEL."
  (let ((items (discourse-state-sequence-list value)))
    (dolist (item items)
      (discourse-api--object item label)
      (discourse-state-id (gethash "id" item)))
    items))

(defun discourse-api--boolean (value label)
  "Return VALUE as boolean or reject it as LABEL."
  (unless (memq value '(nil t :json-false))
    (error "Invalid Discourse %s boolean" label))
  (eq value t))

(defun discourse-api--category-tree (value)
  "Validate category VALUE and its nested subcategory objects."
  (let ((category (discourse-api--object value "category")))
    (discourse-state-id (gethash "id" category))
    (dolist (subcategory
             (discourse-state-sequence-list
              (gethash "subcategory_list" category)))
      (discourse-api--category-tree subcategory))
    category))

(defun discourse-api--site-categories (data)
  "Validate DATA as a Discourse site category catalog."
  (let* ((root (discourse-api--object data "site"))
         (categories
          (discourse-state-sequence-list (gethash "categories" root))))
    (dolist (category categories)
      (discourse-api--category-tree category))
    categories))

(defun discourse-api--site-profile (data)
  "Validate DATA as public Discourse site identity metadata."
  (let* ((profile (discourse-api--object data "site profile"))
         (title (gethash "title" profile))
         (description (gethash "description" profile)))
    (unless (and (stringp title) (not (string-empty-p title)))
      (error "Invalid Discourse site title"))
    (unless (or (null description) (stringp description))
      (error "Invalid Discourse site description"))
    profile))

(defun discourse-api--invalid-result (result error-data)
  "Return invalid-response result based on RESULT and ERROR-DATA."
  (discourse-http-result-create
   :ok-p nil
   :status (discourse-http-result-status result)
   :headers (discourse-http-result-headers result)
   :failure
   (discourse-http-failure-create
    :kind 'invalid-response
    :status (discourse-http-result-status result)
    :message (error-message-string error-data))))

(defun discourse-api--map-result (result transform callback)
  "Apply TRANSFORM to successful RESULT before CALLBACK."
  (if (not (discourse-http-result-ok-p result))
      (funcall callback result)
    (condition-case error-data
        (funcall
         callback
         (discourse-http-result-create
          :ok-p t
          :status (discourse-http-result-status result)
          :headers (discourse-http-result-headers result)
          :data (funcall transform (discourse-http-result-data result))))
      (error
       (funcall callback
                (discourse-api--invalid-result result error-data))))))

(defun discourse-api--topic-page (data)
  "Validate DATA as a Discourse topic page."
  (let* ((root (discourse-api--object data "topic page"))
         (topic-list
          (discourse-api--object
           (gethash "topic_list" root) "topic_list"))
         (topics
          (discourse-api--objects
           (gethash "topics" topic-list) "topic"))
         (users
          (discourse-api--objects (gethash "users" root) "user"))
         (more-url (gethash "more_topics_url" topic-list))
         (can-create-topic
          (discourse-api--boolean
           (gethash "can_create_topic" topic-list)
           "can_create_topic")))
    (unless (or (null more-url)
                (and (stringp more-url)
                     (string-prefix-p "/" more-url)
                     (not (string-prefix-p "//" more-url))))
      (error "Invalid Discourse topic page cursor"))
    (discourse-topic-page-create
     :topics topics
     :users users
     :more-url (and more-url (substring-no-properties more-url))
     :can-create-topic-p can-create-topic)))

(defun discourse-api--topic-snapshot (data)
  "Validate DATA as a complete topic snapshot."
  (let* ((topic (discourse-api--object data "topic"))
         (_topic-id (discourse-state-id (gethash "id" topic)))
         (post-stream
          (discourse-api--object
           (gethash "post_stream" topic) "post_stream"))
         (stream
          (mapcar #'discourse-state-id
                  (discourse-state-sequence-list
                   (gethash "stream" post-stream))))
         (posts
          (discourse-api--objects
           (gethash "posts" post-stream) "post")))
    (discourse-topic-snapshot-create
     :topic topic :stream stream :posts posts)))

(defun discourse-api--post-page (data)
  "Validate DATA as a topic post page."
  (let* ((root (discourse-api--object data "post page"))
         (post-stream
          (discourse-api--object
           (gethash "post_stream" root) "post_stream")))
    (discourse-api--objects (gethash "posts" post-stream) "post")))

(defun discourse-api--current-user (data)
  "Validate DATA as a current-user response and return its user object."
  (let* ((root (discourse-api--object data "current user response"))
         (user
          (discourse-api--object
           (gethash "current_user" root) "current user"))
         (_user-id (discourse-state-id (gethash "id" user)))
         (username (gethash "username" user)))
    (unless (and (stringp username) (not (string-empty-p username)))
      (error "Invalid Discourse current username"))
    user))

(defun discourse-api--posted-object (post)
  "Validate POST as an immediately created post."
  (discourse-api--object post "created post")
  (discourse-state-id (gethash "id" post))
  (discourse-state-id (gethash "topic_id" post))
  (let ((number (gethash "post_number" post)))
    (unless (and (integerp number) (> number 0))
      (error "Invalid Discourse created post number")))
  (discourse-created-post-create :outcome 'posted :post post))

(defun discourse-api--created-post (data)
  "Validate DATA as an immediate or queued create-post response."
  (let ((root (discourse-api--object data "create post response")))
    (cond
     ((gethash "id" root)
      (discourse-api--posted-object root))
     ((hash-table-p (gethash "post" root))
      (discourse-api--posted-object (gethash "post" root)))
     ((equal "enqueued" (gethash "action" root))
      (let ((pending (gethash "pending_post" root))
            (message (gethash "message" root)))
        (unless (or (null pending) (hash-table-p pending))
          (error "Invalid Discourse queued post"))
        (unless (or (null message) (stringp message))
          (error "Invalid Discourse queued post message"))
        (discourse-created-post-create
         :outcome 'queued
         :pending-post pending
         :message (and message (substring-no-properties message)))))
     (t
      (error "Discourse create-post response has no accepted outcome")))))

(defun discourse-api--nonempty-source (value label)
  "Return string VALUE or reject it as empty LABEL."
  (unless (and (stringp value)
               (not (string-empty-p (string-trim value))))
    (user-error "Discourse %s must not be empty" label))
  (substring-no-properties value))

(defun discourse-api--duration (value label)
  "Return nonnegative integer VALUE or reject it as LABEL."
  (unless (and (integerp value) (>= value 0))
    (error "Invalid Discourse %s duration" label))
  value)

(defun discourse-api--create-body
    (raw composer-open-duration typing-duration)
  "Return common create-post body for RAW and timing values."
  (let ((body (make-hash-table :test #'equal)))
    (puthash "raw" (discourse-api--nonempty-source raw "post body") body)
    (puthash "composer_version" 1 body)
    (puthash
     "composer_open_duration_msecs"
     (discourse-api--duration composer-open-duration "composer-open")
     body)
    (puthash
     "typing_duration_msecs"
     (discourse-api--duration typing-duration "typing")
     body)
    body))

(cl-defun discourse-api-topic-page
    (account callback &key (endpoint "/latest.json") owner)
  "Read and validate one topic page for ACCOUNT.

ENDPOINT may be the server-provided `more_topics_url'."
  (discourse-http-get
   account endpoint
   (lambda (result)
     (discourse-api--map-result
      result #'discourse-api--topic-page callback))
   :owner owner))

(cl-defun discourse-api-site-categories (account callback &key owner)
  "Read and validate ACCOUNT's public category catalog."
  (discourse-http-get
   account "/site.json"
   (lambda (result)
     (discourse-api--map-result
      result #'discourse-api--site-categories callback))
   :owner owner))

(cl-defun discourse-api-site-profile (account callback &key owner)
  "Read and validate ACCOUNT's public site identity metadata."
  (discourse-http-get
   account "/site/basic-info.json"
   (lambda (result)
     (discourse-api--map-result
      result #'discourse-api--site-profile callback))
   :owner owner))

(cl-defun discourse-api-topic (account topic-id callback &key owner)
  "Read and validate TOPIC-ID for ACCOUNT."
  (let ((topic-id (discourse-state-id topic-id)))
    (discourse-http-get
     account (format "/t/%s.json" topic-id)
     (lambda (result)
       (discourse-api--map-result
        result #'discourse-api--topic-snapshot callback))
     :owner owner)))

(cl-defun discourse-api-topic-posts
    (account topic-id post-ids callback &key owner)
  "Read and validate POST-IDS from ACCOUNT TOPIC-ID."
  (let ((topic-id (discourse-state-id topic-id))
        (post-ids (mapcar #'discourse-state-id post-ids)))
    (unless post-ids
      (error "Discourse post page requires at least one post ID"))
    (discourse-http-get
     account
     (format "/t/%s/posts.json" topic-id)
     (lambda (result)
       (discourse-api--map-result
        result #'discourse-api--post-page callback))
     :parameters
     (mapcar (lambda (post-id) (cons "post_ids[]" post-id)) post-ids)
     :owner owner)))

(cl-defun discourse-api-current-user (account callback &key owner)
  "Read and validate ACCOUNT's current User API Key identity."
  (discourse-http-get
   account "/session/current.json"
   (lambda (result)
     (discourse-api--map-result
      result #'discourse-api--current-user callback))
   :owner owner))

(defun discourse-api--tags (tags)
  "Return TAGS as a vector of non-empty owned names."
  (unless (proper-list-p tags)
    (error "Discourse tags must be a proper list"))
  (vconcat
   (mapcar
    (lambda (tag)
      (unless (and (stringp tag)
                   (not (string-empty-p (string-trim tag))))
        (error "Invalid Discourse tag"))
      (substring-no-properties (string-trim tag)))
    tags)))

(cl-defun discourse-api-create-topic
    (account title raw callback
             &key category-id tags
             (composer-open-duration 0)
             (typing-duration 0)
             owner)
  "Create a topic for ACCOUNT and validate its accepted result."
  (let ((body
         (discourse-api--create-body
          raw composer-open-duration typing-duration)))
    (puthash "title"
             (discourse-api--nonempty-source title "topic title")
             body)
    (when category-id
      (puthash "category" (discourse-state-id category-id) body))
    (when tags
      ;; Emacs China's pinned Discourse revision accepts scalar tag names.
      (puthash "tags" (discourse-api--tags tags) body))
    (discourse-http-post-json
     account "/posts.json" body
     (lambda (result)
       (discourse-api--map-result
        result #'discourse-api--created-post callback))
     :owner owner)))

(cl-defun discourse-api-create-reply
    (account topic-id raw callback
             &key reply-to-post-number
             (composer-open-duration 0)
             (typing-duration 0)
             owner)
  "Create a reply in ACCOUNT TOPIC-ID and validate its accepted result."
  (let ((body
         (discourse-api--create-body
          raw composer-open-duration typing-duration)))
    (puthash "topic_id" (discourse-state-id topic-id) body)
    (when reply-to-post-number
      (unless (and (integerp reply-to-post-number)
                   (> reply-to-post-number 0))
        (error "Invalid Discourse reply target"))
      (puthash "reply_to_post_number" reply-to-post-number body))
    (discourse-http-post-json
     account "/posts.json" body
     (lambda (result)
       (discourse-api--map-result
        result #'discourse-api--created-post callback))
     :owner owner)))

(provide 'discourse-api)

;;; discourse-api.el ends here
