;;; discourse-topic-test.el --- Topic view contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'discourse-test-helper)
(require 'discourse-runtime)
(require 'discourse-topic)

(defun discourse-topic-test--object (&rest pairs)
  "Return a string-keyed hash table from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest discourse-topic-explicit-position-wins-over-initial-first ()
  (dolist (case '((nil nil "91") (2 nil "92") (nil 2 "92")))
    (let* ((account (discourse-runtime-create-account "https://example.test"))
           (snapshot
            (discourse-topic-snapshot-create
             :topic (discourse-topic-test--object "id" 42 "title" "Position topic")
             :stream '("91" "92")
             :posts
             (list
              (discourse-topic-test--object
               "id" 91 "topic_id" 42 "post_number" 1 "username" "alice"
               "cooked" "<p>First post</p>")
              (discourse-topic-test--object
               "id" 92 "topic_id" 42 "post_number" 2 "username" "bob"
               "cooked" "<p>Second post</p>"))))
           callback buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'discourse-site-ensure-metadata) #'ignore)
                    ((symbol-function 'discourse-media-avatar-image) #'ignore)
                    ((symbol-function 'discourse-api-topic)
                     (lambda (_account _topic-id resolve &rest _arguments)
                       (setq callback resolve) nil)))
            (setq buffer (discourse-topic-open account "42" nil (car case)))
            (discourse-test-drain account)
            (when (cadr case)
              (discourse-topic-open account "42" nil (cadr case)))
            (funcall callback (discourse-http-result-create :ok-p t :data snapshot))
            (discourse-test-drain account)
            (with-current-buffer buffer
              (should (equal (caddr case)
                             (get-text-property (point) discourse-topic-post-id-property)))
              (should-not (discourse-topic-state-target-post-number
                           (appkit-surface-model (appkit-current-surface))))))
        (discourse-runtime-stop-account account)))))

(ert-deftest discourse-topic-reuses-live-view-state ()
  (let
      ((account
        (discourse-runtime-create-account "https://example.test"))
       buffer continued-state)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-topic--request)
              (lambda (&rest _arguments) nil))
             ((symbol-function 'discourse-site-ensure-metadata)
              (lambda (&rest _arguments) nil))
             ((symbol-function 'discourse-topic--continue-target)
              (lambda (_view state) (setq continued-state state))))
          (setq buffer
                (prog1 (discourse-topic-open account "42" nil)
                  (discourse-test-drain account)))
          (let*
              ((view
                (with-current-buffer buffer (appkit-current-surface)))
               (state (appkit-surface-model view)))
            (setf (discourse-topic-state-stream state) '("91")
                  (discourse-topic-state-loaded-p state) t
                  (discourse-topic-state-phase state) 'ready)
            (puthash "91" t (discourse-topic-state-loaded-ids state))
            (should
             (eq buffer
                 (prog1 (discourse-topic-open account "42" nil)
                   (discourse-test-drain account))))
            (should (eq state (appkit-surface-model view)))
            (should
             (equal '("91") (discourse-topic-state-stream state)))
            (should
             (gethash "91" (discourse-topic-state-loaded-ids state)))
            (should
             (eq buffer
                 (prog1 (discourse-topic-open account "42" nil 7)
                   (discourse-test-drain account))))
            (should (eq state continued-state))
            (should
             (= 7 (discourse-topic-state-target-post-number state)))))
      (when (discourse-account-p account)
        (discourse-runtime-stop-account account)))))

(ert-deftest discourse-topic-pages-from-scroll-and-retries-exact-batch
    ()
  (let
      ((account
        (discourse-runtime-create-account "https://example.test"))
       buffer calls)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'discourse-topic--request)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'discourse-site-ensure-metadata)
                (lambda (&rest _arguments) nil)))
            (setq buffer
                  (prog1 (discourse-topic-open account "42" nil)
                    (discourse-test-drain account))))
          (with-current-buffer buffer
            (let*
                ((view (appkit-current-surface))
                 (state (appkit-surface-model view))
                 (failure
                  (discourse-http-result-create :ok-p nil :failure
                                                (discourse-http-failure-create
                                                 :kind 'http :message
                                                 "page failed"))))
              (setf (discourse-topic-state-stream state) '("91" "92")
                    (discourse-topic-state-loaded-p state) t
                    (discourse-topic-state-phase state) 'ready
                    (discourse-topic-state-exhausted-p state) nil)
              (cl-letf
                  (((symbol-function 'discourse-api-topic-posts)
                    (lambda
                      (_account _topic-id post-ids callback &rest
                                options)
                      (push (copy-sequence post-ids) calls)
                      (funcall callback failure) nil)))
                (prog1
                    (discourse-topic--maybe-auto-load view nil
                                                      (point-max)
                                                      (point-max))
                  (discourse-test-drain account))
                (should
                 (equal '("91" "92")
                        (discourse-topic-state-retry-post-ids state)))
                (should (discourse-topic-retry-available-p))
                (prog1 (discourse-topic-retry)
                  (discourse-test-drain account)))
              (should
               (equal '(("91" "92") ("91" "92")) (nreverse calls)))

              )))
      (when (discourse-account-p account)
        (discourse-runtime-stop-account account)))))

(ert-deftest
    discourse-topic-renders-shared-identity-and-actionable-replies ()
  (let*
      ((account
        (discourse-runtime-create-account "https://example.test"))
       (domain-state (discourse-account-state account))
       (category
        (discourse-topic-test--object "id" 5 "name" "General" "color"
                                      "3AB54A"))
       (tag (discourse-topic-test--object "name" "emacs"))
       (topic
        (discourse-topic-test--object "id" 42 "title"
                                      "A coherent topic" "category_id"
                                      5 "tags" (vector tag)))
       (first
        (discourse-topic-test--object "id" 91 "topic_id" 42
                                      "post_number" 1 "user_id" 7
                                      "username" "alice"
                                      "display_username" ""
                                      "avatar_template"
                                      "/alice/{size}.png" "created_at"
                                      "2026-09-02T07:00:00Z" "cooked"
                                      (concat "<p>First post</p>"
                                              "<div class=\"lightbox-wrapper\">"
                                              "<a href=\"https://example.test/original.png\" title=\"Shot\">"
                                              "<img src=\"https://example.test/preview.png\""
                                              " alt=\"Screenshot\" width=\"660\" height=\"500\">"
                                              "<span class=\"informations\">1152×872 111 KB</span>"
                                              "</a></div>"
                                              "<aside class=\"onebox githubrepo\""
                                              " data-onebox-src=\"https://github.com/a/repo\">"
                                              "<header class=\"source\">github.com</header>"
                                              "<article><img src=\"https://img.test/repo\""
                                              " width=\"690\" height=\"344\">"
                                              "<h3>GitHub - a/repo</h3>"
                                              "<p><span class=\"github-repo-description\">"
                                              "A useful package</span></p></article></aside>")
                                      "actions_summary" []))
       (second
        (discourse-topic-test--object "id" 92 "topic_id" 42
                                      "post_number" 2 "user_id" 8
                                      "reply_to_post_number" 1
                                      "username" "bob"
                                      "display_username" ""
                                      "avatar_template"
                                      "/bob/{size}.png" "created_at"
                                      "2026-09-02T08:00:00Z" "cooked"
                                      (concat "<p>Second post</p>"
                                              "<div class=\"lazy-video-container\" data-video-title=\"Watch clip\">"
                                              "<a href=\"https://video.test/watch\">Watch clip</a></div>")
                                      "actions_summary" []))
       buffer)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-media-avatar-demand) #'ignore)
             ((symbol-function 'discourse-media-preview-demand) #'ignore)
             ((symbol-function 'discourse-topic--request)
              (lambda (&rest _arguments) nil))
             ((symbol-function 'discourse-site-ensure-metadata)
              (lambda (&rest _arguments) nil)))
          (setq buffer
                (prog1 (discourse-topic-open account "42" nil)
                  (discourse-test-drain account)))
          (discourse-state-merge-categories domain-state
                                            (list category))
          (discourse-state-set-site-profile domain-state
                                            (discourse-topic-test--object
                                             "title" "Example Forum"))
          (discourse-state-merge-topic domain-state topic)
          (discourse-topic--observe-post-author domain-state first)
          (discourse-topic--observe-post-author domain-state second)
          (discourse-state-merge-post domain-state first)
          (discourse-state-merge-post domain-state second)
          (with-current-buffer buffer
            (let*
                ((view (appkit-current-surface))
                 (state (appkit-surface-model view)))
              (setf (discourse-topic-state-stream state) '("91" "92")
                    (discourse-topic-state-phase state) 'ready
                    (discourse-topic-state-loaded-p state) t
                    (discourse-topic-state-exhausted-p state) t)
              (puthash "91" t (discourse-topic-state-loaded-ids state))
              (puthash "92" t (discourse-topic-state-loaded-ids state))
              (discourse-topic--update-buffer-name view state)
              (prog1
                  (appkit-surface-send view
                                       (appkit-projection-change-create
                                        :full-p t :frame-p t))
                (discourse-test-drain account))
              (let
                  ((text
                    (buffer-substring-no-properties (point-min)
                                                    (point-max)))
                   (header
                    (substring-no-properties
                     (discourse-topic--header-line))))
                (should (string-match-p "Example Forum" header))
                (should (string-match-p "b Latest" header))
                (should (string-match-p "A coherent topic" text))
                (should (string-match-p "General" text))
                (should (string-match-p "#emacs" text))
                (should (string-match-p "alice" text))
                (should (string-match-p "bob" text))
                (should-not (string-match-p "category 5" text))
                (should
                 (string-match-p "\\[image\\] Shot (660×500)" text))
                (should (string-match-p "1152×872 111 KB" text))
                (should (string-match-p "github\\.com" text))
                (should (string-match-p "GitHub - a/repo" text))
                (should (string-match-p "A useful package" text)))
              (save-excursion
                (goto-char (point-min)) (search-forward "First post")
                (let
                    ((body-prefix
                      (get-text-property (1- (point)) 'line-prefix)))
                  (search-forward "[image]")
                  (let
                      ((card-prefix
                        (get-text-property (1- (point)) 'line-prefix)))
                    (should
                     (= (1+ (string-width body-prefix))
                        (string-width card-prefix))))))
              (goto-char (point-min)) (search-forward "[image]")
              (let
                  ((context (appkit-media-card-context-at-point))
                   opened)
                (should (eq 'photo (plist-get context :kind)))
                (cl-letf
                    (((symbol-function
                       'discourse-media-open-image)
                      (lambda (_account url &rest arguments)
                        (setq opened (cons url arguments)))))
                  (appkit-media-card-call-action 'open context))
                (should
                 (equal "https://example.test/original.png"
                        (car opened)))
                (should (eq view (plist-get (cdr opened) :owner))))
              (goto-char (point-min))
              (search-forward "GitHub - a/repo")
              (let ((context (appkit-media-card-context-at-point)))
                (should (eq 'embed (plist-get context :kind)))
                (should (functionp (plist-get context :open-action))))
              (goto-char (point-min)) (search-forward "[Video: Watch clip]")
              (let ((action (get-text-property (1- (point)) appkit-ui-action-property))
                    browsed)
                (cl-letf (((symbol-function 'browse-url)
                           (lambda (url &rest _arguments) (setq browsed url))))
                  (funcall action))
                (should (equal "https://video.test/watch" browsed)))
              (goto-char (point-min)) (search-forward "replying to ")
              (let
                  ((action
                    (get-text-property (point)
                                       appkit-ui-action-property)))
                (should (functionp action))
                (progn
                  (funcall action) (discourse-test-drain account)))
              (should
               (equal "91"
                      (get-text-property (point)
                                         discourse-topic-post-id-property)))
              (should
               (equal '("92") (discourse-topic-state-back-stack state)))
              (prog1 (discourse-topic-jump-back)
                (discourse-test-drain account))
              (should
               (equal "92"
                      (get-text-property (point)
                                         discourse-topic-post-id-property)))

              ))
          (should
           (string-match-p "Example Forum · t/42" (buffer-name buffer))))
      (when (discourse-account-p account)
        (discourse-runtime-stop-account account)))))

(ert-deftest
    discourse-topic-reply-action-captures-post-target-and-capability
    ()
  (let*
      ((account
        (discourse-runtime-create-authenticated-account
         "https://example.test" "7" "writer" "client-7"))
       (domain-state (discourse-account-state account))
       (details (discourse-topic-test--object "can_create_post" t))
       (topic
        (discourse-topic-test--object "id" 42 "title" "Writable topic"
                                      "details" details))
       (post
        (discourse-topic-test--object "id" 91 "topic_id" 42
                                      "post_number" 4 "user_id" 8
                                      "username" "alice" "created_at"
                                      "2026-09-02T08:00:00Z" "cooked"
                                      "<p>Reply body</p>"
                                      "actions_summary" []))
       buffer composed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-topic--request)
              (lambda (&rest _arguments) nil))
             ((symbol-function 'discourse-site-ensure-metadata)
              (lambda (&rest _arguments) nil))
             ((symbol-function 'discourse-compose-reply)
              (lambda (sent-account topic-id &rest options)
                (setq composed (list sent-account topic-id options))
                'compose-buffer)))
          (setq buffer
                (prog1 (discourse-topic-open account "42" nil)
                  (discourse-test-drain account)))
          (discourse-state-merge-topic domain-state topic)
          (discourse-topic--observe-post-author domain-state post)
          (discourse-state-merge-post domain-state post)
          (with-current-buffer buffer
            (let*
                ((view (appkit-current-surface))
                 (state (appkit-surface-model view)))
              (setf (discourse-topic-state-stream state) '("91")
                    (discourse-topic-state-phase state) 'ready
                    (discourse-topic-state-loaded-p state) t
                    (discourse-topic-state-exhausted-p state) t)
              (puthash "91" t (discourse-topic-state-loaded-ids state))
              (prog1
                  (appkit-surface-send view
                                       (appkit-projection-change-create
                                        :full-p t :frame-p t))
                (discourse-test-drain account))
              (goto-char (point-min)) (search-forward "Reply body")
              (should (discourse-topic-can-reply-p))
              (should
               (string-match-p "@writer"
                               (substring-no-properties
                                (discourse-topic--header-line))))
              (discourse-topic-compose-reply)
              (should (eq account (car composed)))
              (should (equal "42" (cadr composed)))
              (let ((options (nth 2 composed)))
                (should
                 (= 4 (plist-get options :reply-to-post-number)))
                (should
                 (equal "alice" (plist-get options :reply-to-username)))
                (should (eq view (plist-get options :source-view)))))))
      (discourse-runtime-stop-account account))))

(provide 'discourse-topic-test)

;;; discourse-topic-test.el ends here
