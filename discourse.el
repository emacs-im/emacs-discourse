;;; discourse.el --- Appkit-based Discourse client -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0") (appkit "0.3.0") (browser-session "0.1.0") (video "0.1.0") (plz "0.8") (transient "0.7"))
;; Keywords: comm

;;; Commentary:

;; Authenticated and anonymous Discourse reading through Appkit-owned
;; applications, stable projections, native markup, and write composition.

;;; Code:

(require 'discourse-api)
(require 'discourse-auth)
(require 'discourse-customize)
(require 'discourse-compose)
(require 'discourse-runtime)
(require 'discourse-topic-list)
(require 'discourse-topic)
(require 'discourse-transient)
(require 'discourse-evil)

(defconst discourse-version "0.1.0"
  "Current discourse.el package version.")

;;;###autoload
(defun discourse (&optional origin)
  "Open the Latest view for Discourse HTTPS ORIGIN."
  (interactive
   (list
    (if current-prefix-arg
        (read-string "Discourse HTTPS origin: " discourse-default-origin)
      discourse-default-origin)))
  (let ((account
         (discourse-runtime-create-account
          (or origin discourse-default-origin))))
    (discourse-topic-list-open-latest account t)))

;;;###autoload
(defun discourse-open-topic (topic-id &optional origin)
  "Open TOPIC-ID from Discourse HTTPS ORIGIN."
  (interactive
   (list (read-string "Discourse topic ID: ")
         (if current-prefix-arg
             (read-string "Discourse HTTPS origin: " discourse-default-origin)
           discourse-default-origin)))
  (let ((account
         (discourse-runtime-create-account
          (or origin discourse-default-origin))))
    (discourse-topic-open account topic-id t)))

(defun discourse--http-failure-message (result)
  "Return a presentation message from failed HTTP RESULT."
  (let ((failure (discourse-http-result-failure result)))
    (if (discourse-http-failure-p failure)
        (discourse-http-failure-message failure)
      "unknown Discourse response failure")))

(defun discourse--open-authenticated-account (account)
  "Validate ACCOUNT's stored key, then open its authenticated Latest view."
  (message "Validating Discourse identity @%s"
           (discourse-account-display-name account))
  (discourse-api-current-user
   account
   (lambda (result)
     (if (not (discourse-http-result-ok-p result))
         (progn
           (message "Discourse login failed: %s"
                    (discourse--http-failure-message result))
           (discourse-runtime-stop-account account))
       (condition-case error-data
           (let* ((user (discourse-http-result-data result))
                  (user-id (discourse-state-id (gethash "id" user)))
                  (username (gethash "username" user))
                  (can-create (gethash "can_create_topic" user))
                  (state (discourse-account-state account)))
             (unless (and (equal user-id
                                 (discourse-account-user-id account))
                          (equal username
                                 (discourse-account-username account)))
               (error "Stored Discourse identity does not match the API key"))
             (discourse-state-merge-user state user)
             (when (memq can-create '(t :json-false))
               (discourse-state-set-can-create-topic
                state (eq can-create t)))
             (message "Opening Discourse as @%s" username)
             (discourse-topic-list-open-latest account t))
         (error
          (discourse-runtime-stop-account account)
          (message "Discourse login failed: %s"
                   (error-message-string error-data))))))
   :owner (discourse-account-app account)))

;;;###autoload
(defun discourse-login (&optional origin reauthorize)
  "Open ORIGIN through a Discourse User API Key account.
Reuse an auth-source identity when available.  With REAUTHORIZE, open the
browser authorization flow even when a stored identity exists."
  (interactive
   (list
    (read-string "Discourse HTTPS origin: " discourse-default-origin)
    current-prefix-arg))
  (setq origin
        (discourse-runtime-normalize-origin
         (or origin discourse-default-origin)))
  (if-let* ((account (and (not reauthorize)
                          (discourse-auth-connect origin))))
      (discourse--open-authenticated-account account)
    (message "Complete Discourse authorization in the opened browser")
    (discourse-auth-authorize
     origin
     (lambda (result)
       (if (discourse-auth-result-ok-p result)
           (discourse--open-authenticated-account
            (discourse-auth-result-account result))
         (message "Discourse authorization failed: %s"
                  (or (discourse-auth-result-message result)
                      "unknown error")))))))
 
;;;###autoload
(defun discourse-disconnect (&optional origin)
  "Stop the anonymous application for Discourse ORIGIN."
  (interactive
   (list (read-string "Discourse HTTPS origin: " discourse-default-origin)))
  (if-let* ((account
             (discourse-runtime-account
              (or origin discourse-default-origin))))
      (discourse-runtime-stop-account account)
    (user-error "No live anonymous Discourse application for this origin")))

(provide 'discourse)

;;; discourse.el ends here
