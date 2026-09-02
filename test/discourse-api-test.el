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

(ert-deftest discourse-api-validates-topic-page-and-server-cursor ()
  (let* ((topic (discourse-api-test--object "id" 42 "title" "Topic"))
         (topic-list
          (discourse-api-test--object
           "topics" (vector topic)
           "more_topics_url" "/latest?page=1"))
         (root (discourse-api-test--object "topic_list" topic-list))
         (page (discourse-api--topic-page root)))
    (should (= 1 (length (discourse-topic-page-topics page))))
    (should (equal "/latest?page=1"
                   (discourse-topic-page-more-url page)))))

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

(provide 'discourse-api-test)

;;; discourse-api-test.el ends here
