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
  identity
  user-id
  username
  client-id
  app
  state)

(defvar discourse-runtime--anonymous-accounts (make-hash-table :test #'equal)
  "Live anonymous accounts keyed by normalized HTTPS origin.")

(defvar discourse-runtime--authenticated-accounts (make-hash-table :test #'equal)
  "Live User API Key accounts keyed by normalized origin and user ID.")

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

(defun discourse-runtime-authenticated-account-id (origin user-id)
  "Return stable authenticated account identity for ORIGIN and USER-ID."
  (list (discourse-runtime-normalize-origin origin)
        :user-api-key
        (discourse-state-id user-id)))

(defun discourse-account-authenticated-p (account)
  "Return non-nil when ACCOUNT uses a Discourse User API Key."
  (and (discourse-account-p account)
       (eq (discourse-account-identity account) 'user-api-key)))

(defun discourse-account-display-name (account)
  "Return ACCOUNT's presentation identity."
  (if (discourse-account-authenticated-p account)
      (discourse-account-username account)
    "anonymous"))

(defun discourse-account-display-identity (account)
  "Return ACCOUNT identity formatted for compact UI presentation."
  (if (discourse-account-authenticated-p account)
      (concat "@" (discourse-account-display-name account))
    (discourse-account-display-name account)))

(defun discourse-runtime--live-account-p (account)
  "Return non-nil when ACCOUNT and its Appkit application are live."
  (and (discourse-account-p account)
       (appkit-app-live-p (discourse-account-app account))))

(defun discourse-runtime--authenticated-key (origin user-id)
  "Return runtime table key for authenticated ORIGIN and USER-ID."
  (list (discourse-runtime-normalize-origin origin)
        (discourse-state-id user-id)))

(defun discourse-runtime-authenticated-account (origin user-id)
  "Return live authenticated ORIGIN account for USER-ID, or nil."
  (let ((account
         (gethash
          (discourse-runtime--authenticated-key origin user-id)
          discourse-runtime--authenticated-accounts)))
    (and (discourse-runtime--live-account-p account) account)))

(defun discourse-runtime-account (origin)
  "Return live anonymous account for ORIGIN, or nil."
  (let* ((normalized (discourse-runtime-normalize-origin origin))
         (account (gethash normalized discourse-runtime--anonymous-accounts)))
    (and (discourse-runtime--live-account-p account) account)))

(defun discourse-runtime-accounts ()
  "Return all live anonymous and User API Key Discourse accounts."
  (let (accounts)
    (dolist (table (list discourse-runtime--anonymous-accounts
                         discourse-runtime--authenticated-accounts))
      (maphash
       (lambda (_key account)
         (when (discourse-runtime--live-account-p account)
           (push account accounts)))
       table))
    (nreverse accounts)))

(defun discourse-runtime--forget-account (account)
  "Remove ACCOUNT from its identity-specific runtime table."
  (if (discourse-account-authenticated-p account)
      (remhash
       (discourse-runtime--authenticated-key
        (discourse-account-origin account)
        (discourse-account-user-id account))
       discourse-runtime--authenticated-accounts)
    (remhash (discourse-account-origin account)
             discourse-runtime--anonymous-accounts)))

(defun discourse-runtime--shutdown (app)
  "Release the account transported by APP."
  (let ((account (appkit-app-transport app)))
    (when (discourse-account-p account)
      (discourse-runtime--forget-account account)
      (setf (discourse-account-app account) nil))))

(appkit-define-app-kind discourse
  :shutdown #'discourse-runtime--shutdown)

(cl-defun discourse-runtime--start-account
    (origin id identity table table-key
            &key user-id username client-id)
  "Start one account application and install it in TABLE under TABLE-KEY."
  (let* ((state (discourse-state-create))
         (account
          (discourse-account--create
           :id id
           :origin origin
           :identity identity
           :user-id user-id
           :username username
           :client-id client-id
           :state state))
         app)
    (condition-case error-data
        (progn
          (setq app
                (appkit-app-start
                 'discourse
                 :id id
                 :state state
                 :transport account))
          (setf (discourse-account-app account) app)
          (puthash table-key account table)
          account)
      (error
       (when (appkit-app-p app)
         (ignore-errors (appkit-app-close app)))
       (remhash table-key table)
       (signal (car error-data) (cdr error-data))))))

(defun discourse-runtime-create-account (origin)
  "Create or reuse the anonymous account for HTTPS ORIGIN."
  (let* ((origin (discourse-runtime-normalize-origin origin))
         (existing (gethash origin discourse-runtime--anonymous-accounts)))
    (if (discourse-runtime--live-account-p existing)
        existing
      (discourse-runtime--start-account
       origin
       (discourse-runtime-account-id origin)
       'anonymous
       discourse-runtime--anonymous-accounts
       origin))))

(defun discourse-runtime-create-authenticated-account
    (origin user-id username client-id)
  "Create or reuse USER-ID's User API Key account for HTTPS ORIGIN."
  (setq origin (discourse-runtime-normalize-origin origin)
        user-id (discourse-state-id user-id))
  (unless (and (stringp username)
               (not (string-empty-p username))
               (stringp client-id)
               (not (string-empty-p client-id)))
    (error "Authenticated Discourse identity is incomplete"))
  (let* ((key (discourse-runtime--authenticated-key origin user-id))
         (existing
          (gethash key discourse-runtime--authenticated-accounts)))
    (cond
     ((and (discourse-runtime--live-account-p existing)
           (equal username (discourse-account-username existing))
           (equal client-id (discourse-account-client-id existing)))
      existing)
     (t
      (when (discourse-runtime--live-account-p existing)
        (discourse-runtime-stop-account existing))
      (discourse-runtime--start-account
       origin
       (discourse-runtime-authenticated-account-id origin user-id)
       'user-api-key
       discourse-runtime--authenticated-accounts
       key
       :user-id user-id
       :username (substring-no-properties username)
       :client-id (substring-no-properties client-id))))))

(defun discourse-runtime-stop-account (account)
  "Stop ACCOUNT and every Appkit-owned resource beneath it."
  (when (discourse-account-p account)
    (if-let* ((app (discourse-account-app account)))
        (appkit-app-close app)
      (discourse-runtime--forget-account account))
    t))

(defun discourse-runtime-stop-all ()
  "Stop every live anonymous and authenticated Discourse account."
  (dolist (account (discourse-runtime-accounts))
    (discourse-runtime-stop-account account)))

(provide 'discourse-runtime)

;;; discourse-runtime.el ends here
