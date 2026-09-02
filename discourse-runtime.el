;;; discourse-runtime.el --- Appkit runtime for discourse.el -*- lexical-binding: t; -*-

;;; Commentary:

;; One normalized Discourse origin owns one anonymous Appkit application.  A
;; later authenticated account uses a distinct identity and cannot share this
;; canonical state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'discourse-state)

(cl-defstruct (discourse-account
               (:constructor discourse-account--create)
               (:copier nil))
  id
  origin
  app
  state)

(defvar discourse-runtime--anonymous-accounts (make-hash-table :test #'equal)
  "Live anonymous accounts keyed by normalized HTTPS origin.")

(defun discourse-runtime-normalize-origin (origin)
  "Return validated, normalized HTTPS ORIGIN."
  (unless (and (stringp origin)
               (not (string-empty-p (string-trim origin))))
    (user-error "Discourse origin must not be empty"))
  (let* ((parsed (url-generic-parse-url (string-trim origin)))
         (host (and (url-host parsed) (downcase (url-host parsed))))
         (port (url-port parsed)))
    (unless (and (equal (url-type parsed) "https")
                 (stringp host)
                 (not (string-empty-p host))
                 (not (string-match-p "[^A-Za-z0-9.:-]" host))
                 (null (url-user parsed))
                 (null (url-password parsed))
                 (member (url-filename parsed) '("" "/"))
                 (null (url-target parsed)))
      (user-error
       "Discourse instance must be an HTTPS origin without credentials, path, query, or fragment"))
    (concat "https://" host
            (if (= port 443) "" (format ":%d" port)))))

(defun discourse-runtime-account-id (origin)
  "Return stable anonymous account identity for ORIGIN."
  (list (discourse-runtime-normalize-origin origin) :anonymous))

(defun discourse-runtime-account (origin)
  "Return live anonymous account for ORIGIN, or nil."
  (let* ((normalized (discourse-runtime-normalize-origin origin))
         (account (gethash normalized discourse-runtime--anonymous-accounts)))
    (and (discourse-account-p account)
         (appkit-app-live-p (discourse-account-app account))
         account)))

(defun discourse-runtime-accounts ()
  "Return all live anonymous Discourse accounts."
  (let (accounts)
    (maphash
     (lambda (_origin account)
       (when (and (discourse-account-p account)
                  (appkit-app-live-p (discourse-account-app account)))
         (push account accounts)))
     discourse-runtime--anonymous-accounts)
    (nreverse accounts)))

(defun discourse-runtime--shutdown (app)
  "Release the anonymous account transported by APP."
  (let ((account (appkit-app-transport app)))
    (when (discourse-account-p account)
      (remhash (discourse-account-origin account)
               discourse-runtime--anonymous-accounts)
      (setf (discourse-account-app account) nil))))

(appkit-define-app-kind discourse
  :shutdown #'discourse-runtime--shutdown)

(defun discourse-runtime-create-account (origin)
  "Create or reuse the anonymous account for HTTPS ORIGIN."
  (let* ((origin (discourse-runtime-normalize-origin origin))
         (existing (gethash origin discourse-runtime--anonymous-accounts)))
    (if (and (discourse-account-p existing)
             (appkit-app-live-p (discourse-account-app existing)))
        existing
      (let* ((state (discourse-state-create))
             (account
              (discourse-account--create
               :id (list origin :anonymous)
               :origin origin
               :state state))
             app)
        (condition-case error-data
            (progn
              (setq app
                    (appkit-start-app
                     'discourse
                     :id (discourse-account-id account)
                     :state state
                     :transport account))
              (setf (discourse-account-app account) app)
              (puthash origin account discourse-runtime--anonymous-accounts)
              account)
          (error
           (when (appkit-app-p app)
             (ignore-errors (appkit-stop-app app)))
           (remhash origin discourse-runtime--anonymous-accounts)
           (signal (car error-data) (cdr error-data))))))))

(defun discourse-runtime-stop-account (account)
  "Stop ACCOUNT and every Appkit-owned resource beneath it."
  (when (discourse-account-p account)
    (if-let* ((app (discourse-account-app account)))
        (appkit-stop-app app)
      (remhash (discourse-account-origin account)
               discourse-runtime--anonymous-accounts))
    t))

(defun discourse-runtime-stop-all ()
  "Stop every live anonymous Discourse account."
  (dolist (account (discourse-runtime-accounts))
    (discourse-runtime-stop-account account)))

(provide 'discourse-runtime)

;;; discourse-runtime.el ends here
