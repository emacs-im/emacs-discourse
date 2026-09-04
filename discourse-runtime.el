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
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'discourse-state)

(cl-defstruct (discourse-account
               (:constructor discourse-account--create)
               (:copier nil))
  id origin identity user-id username client-id app state
  (resources (make-hash-table :test #'equal))
  (composers (make-hash-table :test #'equal)))

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
  "Release the account owned by APP."
  (let ((account (appkit-app-model app)))
    (discourse-runtime--forget-account account)
    (clrhash (discourse-account-resources account))
    (setf (discourse-account-app account) nil)))

(defconst discourse-runtime--app-type
  (appkit-app-type-create
   :name 'discourse
   :init (lambda (_context account)
           (appkit-next :model account :render appkit-render-none))
   :update #'discourse-runtime--account-update
   :shutdown #'discourse-runtime--shutdown))

(cl-defun discourse-runtime--start-account
    (origin id identity table table-key &key user-id username
            client-id)
  "Start one account application and install it in TABLE under TABLE-KEY."
  (let*
      ((state (discourse-state-create))
       (account
        (discourse-account--create :id id :origin origin :identity
                                   identity :user-id user-id :username
                                   username :client-id client-id
                                   :state state))
       app)
    (condition-case error-data
        (progn
          (setq app
                (appkit-app-start discourse-runtime--app-type
                                  :identity id :input account))
          (setf (discourse-account-app account) app)
          (puthash table-key account table) account)
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

(defvar discourse-runtime--transition-context nil
  "Context whose domain notifications become closed commands.")

(defvar discourse-runtime--commands nil
  "Reverse-ordered closed commands collected by the current transition.")

(defvar-local discourse-runtime--surface-address nil
  "Opaque address captured from this Surface's initialization context.")

(defun discourse-runtime--surface-init (context state)
  "Initialize STATE and retain this exact Surface's routing capability."
  (setq-local discourse-runtime--surface-address
              (appkit-transition-context-owner-address context))
  (appkit-next :model state
               :render (appkit-projection-change-create
                        :full-p t :frame-p t :position 'first)))

(defun discourse-runtime--post-surface (surface message)
  "Deliver MESSAGE externally, or stage a closed transition post to SURFACE."
  (if discourse-runtime--transition-context
      (push (appkit-command-post-message
             :target (buffer-local-value 'discourse-runtime--surface-address
                                         (appkit-surface-buffer surface))
             :message message :delivery 'report)
            discourse-runtime--commands)
    (appkit-surface-post surface message)))

(defun discourse-runtime--surface-update (context model message)
  "Commit projection requests and request Effects for a generated host."
  (let ((discourse-runtime--transition-context context)
        discourse-runtime--commands)
    (pcase message
      ((pred appkit-projection-change-p)
       (appkit-next :model model :render message))
      (`(start-effect ,effect)
       (appkit-next :model model :render appkit-render-none
                    :commands (list (appkit-command-start-effect effect))))
      (`(response ,handler ,result)
       (funcall handler result)
       (appkit-next :model model :render appkit-render-none
                    :commands (nreverse discourse-runtime--commands)))
      (_ (discourse-media--update model message)))))

(defun discourse-runtime--request-effect (surface key start handler)
  "Run START as a replaceable request Effect owned by SURFACE.\nSTART receives a settlement function; HANDLER runs in the committed loop."
  (discourse-runtime--post-surface surface
                                   (list 'start-effect
                                         (appkit-effect-create :key key :input nil
                                                               :start
                                                               (lambda
                                                                 (_context _input
                                                                           _observe
                                                                           resolve
                                                                           _reject)
                                                                 (let
                                                                     ((request
                                                                        (funcall
                                                                         start
                                                                         resolve)))
                                                                   (appkit-cancellation-create
                                                                    :kind
                                                                    'transport
                                                                    :cancel
                                                                    (lambda ()
                                                                      (discourse-http-cancel
                                                                       request)))))
                                                               :success
                                                               (lambda
                                                                 (_input result)
                                                                 (list 'response
                                                                       handler
                                                                       result))
                                                               :failure
                                                               (lambda
                                                                 (_input result)
                                                                 (list 'response
                                                                       handler
                                                                       result))
                                                               :cancellation-requirement
                                                               'transport))))

(defun discourse-runtime--account-update (_context account _message)
  "Retain ACCOUNT while its Resource coordinator commits image deliveries."
  (appkit-next :model account :render appkit-render-none))

(provide 'discourse-runtime)

;;; discourse-runtime.el ends here
