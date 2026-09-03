;;; discourse-auth-test.el --- User API Key contracts for discourse.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'discourse-auth)
(require 'discourse-runtime)

(ert-deftest discourse-auth-authorization-requests-least-write-scopes ()
  (let ((url
         (discourse-auth--authorization-url
          "https://example.test"
          "-----BEGIN PUBLIC KEY-----\npublic\n-----END PUBLIC KEY-----\n"
          "emacs-discourse-client"
          "nonce-value")))
    (should (string-prefix-p
             "https://example.test/user-api-key/new?" url))
    (should (string-match-p "scopes=write%2Csession_info" url))
    (should (string-match-p "padding=oaep" url))
    (should-not (string-match-p "auth_redirect" url))
    (should (string-match-p "user-api-key-payload"
                            discourse-auth--page-script))
    (should (string-match-p "session/current.json"
                            discourse-auth--page-script))
    (should-not (string-match-p "document\\.cookie"
                                discourse-auth--page-script))))

(ert-deftest discourse-auth-rsa-oaep-round-trip-verifies-nonce ()
  (let* ((directory (make-temp-file "discourse-auth-test-" t))
         (discourse-auth-directory directory)
         (origin "https://example.test")
         (nonce "0123456789abcdef")
         (api-key "0123456789abcdef0123456789abcdef")
         (private-key (discourse-auth--private-key-file origin))
         (public-key (discourse-auth--public-key private-key))
         (public-file (make-temp-file "discourse-public-"))
         (plain-file (make-temp-file "discourse-plain-"))
         encrypted)
    (unwind-protect
        (progn
          (with-temp-file public-file (insert public-key))
          (with-temp-file plain-file
            (insert
             (json-serialize
              `((key . ,api-key)
                (nonce . ,nonce)
                (push . :json-false)
                (api . 4))
              :false-object :json-false)))
          (setq encrypted
                (base64-encode-string
                 (discourse-auth--openssl-output
                  "pkeyutl" "-encrypt" "-pubin"
                  "-inkey" public-file
                  "-pkeyopt" "rsa_padding_mode:oaep"
                  "-in" plain-file)
                 t))
          (should
           (equal api-key
                  (discourse-auth--decrypt-payload
                   encrypted private-key nonce)))
          (should-error
           (discourse-auth--decrypt-payload
            encrypted private-key "wrong-nonce")))
      (ignore-errors (delete-file public-file))
      (ignore-errors (delete-file plain-file))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest discourse-auth-api-key-is-resolved-from-exact-auth-source-token ()
  (let ((account
         (discourse-account--create
          :origin "https://example.test"
          :identity 'user-api-key
          :user-id "7"
          :username "alice"
          :client-id "client-7")))
    (cl-letf (((symbol-function 'discourse-auth--source-tokens)
               (lambda (_origin &optional _username)
                 (list
                  (list :user "alice"
                        :user-id "7"
                        :client-id "different"
                        :secret (lambda () "ffffffffffffffffffffffffffffffff"))
                  (list :user "alice"
                        :user-id "7"
                        :client-id "client-7"
                        :secret
                        (lambda () "0123456789abcdef0123456789abcdef"))))))
      (should
       (equal "0123456789abcdef0123456789abcdef"
              (discourse-auth-api-key account))))))



(ert-deftest discourse-auth-browser-capture-returns-page-result-not-cookies ()
  (let ((directory (make-temp-file "discourse-browser-auth-" t))
        captured
        request)
    (unwind-protect
        (let ((discourse-auth-directory directory))
          (cl-letf
              (((symbol-function 'discourse-auth--private-key-file)
                (lambda (_origin) "/private/client.pem"))
               ((symbol-function 'discourse-auth--public-key)
                (lambda (_file) "PUBLIC"))
               ((symbol-function 'discourse-auth--nonce)
                (lambda () "nonce"))
               ((symbol-function 'browser-session-capture)
                (lambda (&rest arguments)
                  (setq captured arguments)
                  'browser-request)))
            (setq request
                  (discourse-auth-authorize
                   "https://example.test" #'ignore))
            (should (equal discourse-auth--page-script
                           (plist-get captured :script)))
            (should-not (plist-member captured :cookies))
            (should-not (plist-member captured :all-origin-cookies))
            (should (string-match-p
                     "/user-api-key/new?"
                     (plist-get captured :url)))
            (should (discourse-auth-cancel request))))
      (when (discourse-auth-request-p request)
        (when-let* ((owner (discourse-auth-request-owner request))
                    ((appkit-app-live-p owner)))
          (appkit-stop-app owner)))
      (ignore-errors (delete-directory directory t)))))
(ert-deftest discourse-auth-source-round-trip-persists-identity-metadata ()
  (let* ((directory (make-temp-file "discourse-auth-source-" t))
         (file (expand-file-name "authinfo" directory))
         (auth-sources (list file))
         (auth-source-save-behavior t)
         (auth-source-gpg-encrypt-to nil)
         (auth-source-netrc-use-gpg-tokens 'never)
         (auth-source-do-cache nil)
         (key "0123456789abcdef0123456789abcdef")
         account)
    (unwind-protect
        (progn
          (write-region "" nil file nil 'silent)
          (set-file-modes file #o600)
          (auth-source-forget-all-cached)
          (discourse-auth--store-api-key
           "https://example.test" "7" "alice" "client-7" key)
          (setq account
                (discourse-auth-connect
                 "https://example.test" "alice"))
          (should (discourse-account-authenticated-p account))
          (should (equal "7" (discourse-account-user-id account)))
          (should (equal "client-7"
                         (discourse-account-client-id account)))
          (should (equal key (discourse-auth-api-key account)))
          (should (= #o600 (logand #o777 (file-modes file)))))
      (when account (discourse-runtime-stop-account account))
      (auth-source-forget-all-cached)
      (ignore-errors (delete-directory directory t)))))

(ert-deftest discourse-runtime-separates-anonymous-and-authenticated-state ()
  (let ((anonymous
         (discourse-runtime-create-account "https://example.test"))
        (authenticated
         (discourse-runtime-create-authenticated-account
          "https://example.test" "7" "alice" "client-7")))
    (unwind-protect
        (progn
          (should-not (eq anonymous authenticated))
          (should-not (eq (discourse-account-state anonymous)
                          (discourse-account-state authenticated)))
          (should (equal "anonymous"
                         (discourse-account-display-identity anonymous)))
          (should (equal "@alice"
                         (discourse-account-display-identity authenticated)))
          (should
           (eq authenticated
               (discourse-runtime-authenticated-account
                "https://example.test" "7"))))
      (discourse-runtime-stop-account anonymous)
      (discourse-runtime-stop-account authenticated))))

(provide 'discourse-auth-test)

;;; discourse-auth-test.el ends here
