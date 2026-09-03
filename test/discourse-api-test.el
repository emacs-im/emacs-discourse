;;; discourse-api-test.el --- API contracts for discourse.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'discourse-api)
(require 'discourse-http)
(require 'discourse-runtime)

(defun discourse-api-test--object (&rest pairs)
  "Return a string-keyed hash table from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest discourse-http-preserves-repeated-query-parameters ()
  (should
   (equal
    "post_ids%5B%5D=11&post_ids%5B%5D=12"
    (discourse-http-encode-parameters
     '(("post_ids[]" . "11") ("post_ids[]" . "12"))))))

(ert-deftest discourse-http-appends-to-server-provided-cursor-query ()
  (let ((account
         (discourse-account--create
          :origin "https://example.test")))
    (should
     (equal
      "https://example.test/latest?page=2&ascending=true"
      (discourse-http--endpoint-url
       account "/latest?page=2" '((ascending . t)))))))

(ert-deftest discourse-api-validates-topic-page-users-and-server-cursor ()
  (let* ((topic (discourse-api-test--object "id" 42 "title" "Topic"))
         (user (discourse-api-test--object "id" 7 "username" "alice"))
         (topic-list
          (discourse-api-test--object
           "topics" (vector topic)
           "more_topics_url" "/latest?page=1"))
         (root
          (discourse-api-test--object
           "users" (vector user)
           "topic_list" topic-list))
         (page (discourse-api--topic-page root)))
    (should (= 1 (length (discourse-topic-page-topics page))))
    (should (= 1 (length (discourse-topic-page-users page))))
    (should (equal "/latest?page=1"
                   (discourse-topic-page-more-url page)))))

(ert-deftest discourse-api-validates-public-site-metadata ()
  (let* ((child
          (discourse-api-test--object "id" 6 "name" "Child"))
         (category
          (discourse-api-test--object
           "id" 5
           "name" "General"
           "subcategory_list" (vector child)))
         (site
          (discourse-api-test--object "categories" (vector category)))
         (profile
          (discourse-api-test--object
           "title" "Example Forum"
           "description" "Example description")))
    (should (= 1 (length (discourse-api--site-categories site))))
    (should (eq profile (discourse-api--site-profile profile)))
    (should-error
     (discourse-api--site-profile
      (discourse-api-test--object "title" "")))))

(ert-deftest discourse-api-validates-topic-stream-identities ()
  (let* ((post (discourse-api-test--object "id" 91 "topic_id" 42))
         (post-stream
          (discourse-api-test--object
           "stream" [91 "92"]
           "posts" (vector post)))
         (topic
          (discourse-api-test--object
           "id" 42
           "title" "Topic"
           "post_stream" post-stream))
         (snapshot (discourse-api--topic-snapshot topic)))
    (should (equal '("91" "92")
                   (discourse-topic-snapshot-stream snapshot)))
    (should (= 1 (length (discourse-topic-snapshot-posts snapshot))))))

(ert-deftest discourse-api-rejects-cross-origin-page-cursor ()
  (let* ((topic-list
          (discourse-api-test--object
           "topics" []
           "more_topics_url" "https://attacker.test/latest"))
         (root (discourse-api-test--object "topic_list" topic-list)))
    (should-error (discourse-api--topic-page root))))


(ert-deftest discourse-http-authenticated-post-uses-json-and-never-retries ()
  (let ((account
         (discourse-runtime-create-authenticated-account
          "https://example.test" "7" "alice" "client-7"))
        captured
        result
        (retries 0))
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-auth-api-key)
              (lambda (_account)
                "0123456789abcdef0123456789abcdef"))
             ((symbol-function 'discourse-http--schedule-retry)
              (lambda (&rest _arguments) (cl-incf retries)))
             ((symbol-function 'plz)
              (lambda (method url &rest arguments)
                (setq captured (list method url arguments))
                (funcall
                 (plist-get arguments :else)
                 (make-plz-error
                  :response
                  (make-plz-response
                   :status 429
                   :headers '(("Retry-After" . "3"))
                   :body "{\"errors\":[\"slow down\"]}")))
                'fake-process)))
          (let ((body (make-hash-table :test #'equal)))
            (puthash "raw" "hello" body)
            (discourse-http-post-json
             account "/posts.json" body
             (lambda (value) (setq result value))))
          (should (eq 'post (car captured)))
          (should (equal "https://example.test/posts.json"
                         (cadr captured)))
          (let* ((arguments (caddr captured))
                 (headers (plist-get arguments :headers))
                 (body
                  (json-parse-string
                   (plist-get arguments :body)
                   :object-type 'hash-table)))
            (should (equal "0123456789abcdef0123456789abcdef"
                           (cdr (assoc "User-Api-Key" headers))))
            (should (equal "client-7"
                           (cdr (assoc "User-Api-Client-Id" headers))))
            (should (equal "hello" (gethash "raw" body))))
          (should (zerop retries))
          (should-not (discourse-http-result-ok-p result))
          (should (= 429 (discourse-http-result-status result)))
          (should (= 3
                     (discourse-http-failure-retry-after
                      (discourse-http-result-failure result)))))
      (discourse-runtime-stop-account account))))

(ert-deftest discourse-api-create-reply-sends-exact-wire-contract ()
  (let ((account
         (discourse-runtime-create-authenticated-account
          "https://example.test" "7" "alice" "client-7"))
        captured
        accepted)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-http-post-json)
              (lambda (_account endpoint data callback &rest arguments)
                (setq captured (list endpoint data arguments))
                (funcall
                 callback
                 (discourse-http-result-create
                  :ok-p t
                  :status 200
                  :data
                  (discourse-api-test--object
                   "id" 91
                   "topic_id" 42
                   "post_number" 3
                   "raw" "exact **Markdown**")))
                'request)))
          (discourse-api-create-reply
           account "42" "exact **Markdown**"
           (lambda (result)
             (setq accepted (discourse-http-result-data result)))
           :reply-to-post-number 2
           :composer-open-duration 1200
           :typing-duration 300)
          (should (equal "/posts.json" (car captured)))
          (let ((body (cadr captured)))
            (should (equal "42" (gethash "topic_id" body)))
            (should (= 2 (gethash "reply_to_post_number" body)))
            (should (= 1 (gethash "composer_version" body)))
            (should (= 1200
                       (gethash "composer_open_duration_msecs" body)))
            (should (= 300 (gethash "typing_duration_msecs" body)))
            (should (equal "exact **Markdown**" (gethash "raw" body))))
          (should (eq 'posted
                      (discourse-created-post-outcome accepted)))
          (should (equal 91
                         (gethash
                          "id" (discourse-created-post-post accepted)))))
      (discourse-runtime-stop-account account))))

(ert-deftest discourse-api-create-topic-preserves-queued-acceptance ()
  (let ((queued
         (discourse-api--created-post
          (discourse-api-test--object
           "action" "enqueued"
           "pending_post"
           (discourse-api-test--object "id" 12 "raw" "awaiting review")
           "message" "Queued for review"))))
    (should (eq 'queued (discourse-created-post-outcome queued)))
    (should (equal "Queued for review"
                   (discourse-created-post-message queued)))
    (should (= 12
               (gethash "id"
                        (discourse-created-post-pending-post queued))))))

(ert-deftest discourse-api-validates-current-user-and-create-capability ()
  (let* ((user
          (discourse-api-test--object
           "id" 7 "username" "alice" "can_create_topic" t))
         (root (discourse-api-test--object "current_user" user))
         (topic-list
          (discourse-api-test--object
           "topics" [] "can_create_topic" t))
         (page
          (discourse-api--topic-page
           (discourse-api-test--object
            "users" [] "topic_list" topic-list))))
    (should (eq user (discourse-api--current-user root)))
    (should (discourse-topic-page-can-create-topic-p page))))
(provide 'discourse-api-test)

;;; discourse-api-test.el ends here
