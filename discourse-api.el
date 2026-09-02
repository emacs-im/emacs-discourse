;;; discourse-api.el --- Discourse anonymous API contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Validate the three anonymous response shapes used by the first vertical
;; slice.  Views never inspect unvalidated top-level JSON structure.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'discourse-http)
(require 'discourse-state)

(cl-defstruct (discourse-topic-page
               (:constructor discourse-topic-page-create)
               (:copier nil))
  topics
  more-url)

(cl-defstruct (discourse-topic-snapshot
               (:constructor discourse-topic-snapshot-create)
               (:copier nil))
  topic
  stream
  posts)

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
         (more-url (gethash "more_topics_url" topic-list)))
    (unless (or (null more-url)
                (and (stringp more-url)
                     (string-prefix-p "/" more-url)
                     (not (string-prefix-p "//" more-url))))
      (error "Invalid Discourse topic page cursor"))
    (discourse-topic-page-create
     :topics topics
     :more-url (and more-url (substring-no-properties more-url)))))

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

(provide 'discourse-api)

;;; discourse-api.el ends here
