;;; discourse-compose-test.el --- Write composition contracts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'discourse-compose)
(require 'discourse-runtime)
(require 'discourse-state)

(defun discourse-compose-test--object (&rest pairs)
  "Return a string-keyed hash table from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(defun discourse-compose-test--account ()
  "Return a live authenticated account with explicit write capabilities."
  (let* ((account
          (discourse-runtime-create-authenticated-account
           "https://example.test" "7" "alice" "client-7"))
         (state (discourse-account-state account))
         (category
          (discourse-compose-test--object
           "id" 5
           "name" "General"
           "permission" 1
           "minimum_required_tags" 1
           "topic_template" "## Context\n\n"))
         (details
          (discourse-compose-test--object "can_create_post" t))
         (topic
          (discourse-compose-test--object
           "id" 42 "title" "Existing" "details" details)))
    (discourse-state-set-can-create-topic state t)
    (discourse-state-merge-categories state (list category))
    (discourse-state-merge-topic state topic)
    account))

(defun discourse-compose-test--cleanup (buffer account)
  "Release compose BUFFER and ACCOUNT without interactive prompts."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local discourse-compose--accepted-p t)
      (set-buffer-modified-p nil))
    (kill-buffer buffer))
  (when (discourse-account-p account)
    (discourse-runtime-stop-account account)))

(ert-deftest discourse-compose-new-topic-captures-metadata-and-raw-markdown ()
  (let ((account (discourse-compose-test--account))
        buffer)
    (unwind-protect
        (progn
          (setq buffer
                (discourse-compose-new-topic
                 account
                 :title "A real topic"
                 :category-id "5"
                 :tags '("emacs")
                 :select nil))
          (with-current-buffer buffer
            (should (derived-mode-p 'discourse-compose-mode))
            (should (equal "## Context\n\n" (buffer-string)))
            (should (= 0 (appkit-compose-generation)))
            (goto-char (point-max))
            (insert "Body with **Markdown**")
            (should (= 1 (appkit-compose-generation)))
            (should (= 100 discourse-compose-typing-duration))
            (let* ((capture (appkit-compose-capture))
                   (draft (plist-get capture :value)))
              (should (equal "A real topic" (plist-get draft :title)))
              (should (equal "5" (plist-get draft :category-id)))
              (should (equal '("emacs") (plist-get draft :tags)))
              (should
               (equal "## Context\n\nBody with **Markdown**"
                      (plist-get draft :raw))))
            (let ((header (discourse-compose--header-line)))
              (should (string-match-p "@alice" header))
              (should (string-match-p "A real topic" header))
              (should (string-match-p "General" header)))))
      (discourse-compose-test--cleanup buffer account))))

(ert-deftest discourse-compose-requires-server-capability ()
  (let* ((account
          (discourse-runtime-create-authenticated-account
           "https://denied.test" "8" "bob" "client-8"))
         (state (discourse-account-state account))
         (details
          (discourse-compose-test--object "can_create_post" :json-false))
         (topic
          (discourse-compose-test--object
           "id" 44 "title" "Closed" "details" details)))
    (unwind-protect
        (progn
          (discourse-state-set-can-create-topic state nil)
          (discourse-state-merge-topic state topic)
          (should-not (discourse-compose-new-topic-allowed-p account))
          (should-not (discourse-compose-reply-allowed-p account "44"))
          (should-error
           (discourse-compose-new-topic account :select nil)
           :type 'user-error)
          (should-error
           (discourse-compose-reply account "44" :select nil)
           :type 'user-error))
      (discourse-runtime-stop-account account))))

(ert-deftest discourse-compose-submit-posts-once-and-reconciles-server-id ()
  (let ((account (discourse-compose-test--account))
        buffer
        callback
        captured
        opened)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-api-create-reply)
              (lambda (sent-account topic-id raw sent-callback &rest options)
                (setq captured
                      (list sent-account topic-id raw options)
                      callback sent-callback)
                'request))
             ((symbol-function 'discourse-topic-open)
              (lambda (&rest arguments)
                (setq opened arguments)
                (get-buffer-create " *discourse-compose-opened*"))))
          (setq buffer
                (discourse-compose-reply
                 account "42"
                 :reply-to-post-number 2
                 :reply-to-username "originator"
                 :select nil))
          (with-current-buffer buffer
            (insert "Exact reply source")
            (discourse-compose-submit)
            (should (appkit-compose-operation-active-p))
            (should buffer-read-only))
          (should (eq account (car captured)))
          (should (equal "42" (cadr captured)))
          (should (equal "Exact reply source" (nth 2 captured)))
          (should (equal 2
                         (plist-get (nth 3 captured)
                                    :reply-to-post-number)))
          (let ((post
                 (discourse-compose-test--object
                  "id" 91
                  "topic_id" 42
                  "post_number" 3
                  "raw" "Exact reply source"
                  "cooked" "<p>Exact reply source</p>")))
            (funcall
             callback
             (discourse-http-result-create
              :ok-p t
              :status 200
              :data
              (discourse-created-post-create
               :outcome 'posted :post post))))
          (should-not (buffer-live-p buffer))
          (should (equal (list account "42" t 3) opened))
          (should
           (equal "Exact reply source"
                  (gethash
                   "raw"
                   (discourse-state-post
                    (discourse-account-state account) "91")))))
      (when-let* ((opened-buffer (get-buffer " *discourse-compose-opened*")))
        (kill-buffer opened-buffer))
      (discourse-compose-test--cleanup buffer account))))

(ert-deftest discourse-compose-transport-failure-remains-editable-and-uncertain ()
  (let ((account (discourse-compose-test--account))
        buffer)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-api-create-reply)
              (lambda (_account _topic-id _raw callback &rest _options)
                (funcall
                 callback
                 (discourse-http-result-create
                  :ok-p nil
                  :status 0
                  :failure
                  (discourse-http-failure-create
                   :kind 'transport
                   :status 0
                   :message "connection ended")))
                nil)))
          (setq buffer
                (discourse-compose-reply account "42" :select nil))
          (with-current-buffer buffer
            (insert "Keep this draft")
            (discourse-compose-submit)
            (should-not buffer-read-only)
            (should-not (appkit-compose-operation-active-p))
            (should (eq 'unknown discourse-compose-write-outcome))
            (should (equal "connection ended" discourse-compose-message))
            (should (equal "Keep this draft" (buffer-string)))))
      (discourse-compose-test--cleanup buffer account))))

(ert-deftest discourse-compose-classifies-only-definitive-rejections-as-safe ()
  (let ((rejected
         (discourse-http-result-create
          :ok-p nil
          :status 422
          :failure
          (discourse-http-failure-create
           :kind 'http :status 422 :message "invalid")))
        (unknown
         (discourse-http-result-create
          :ok-p nil
          :status 200
          :failure
          (discourse-http-failure-create
           :kind 'invalid-response :status 200 :message "bad response"))))
    (should-not (discourse-compose--unknown-result-p rejected))
    (should (discourse-compose--unknown-result-p unknown))))

(provide 'discourse-compose-test)

;;; discourse-compose-test.el ends here
