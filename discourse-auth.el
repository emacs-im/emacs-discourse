;;; discourse-auth.el --- Discourse User API Key authorization -*- lexical-binding: t; -*-

;;; Commentary:

;; Browser-assisted User API Key authorization.  browser-session owns the
;; isolated login browser and returns only the encrypted authorization result;
;; browser cookies never become API credentials or enter this package.  The
;; resulting per-user API key is persisted through auth-source.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'appkit-core)
(require 'browser-session)
(require 'discourse-customize)
(require 'discourse-runtime)
(require 'discourse-state)

(define-error 'discourse-auth-credential-error
              "Discourse User API Key is unavailable")

(defconst discourse-auth--source-port "discourse-user-api"
  "auth-source service name for Discourse User API keys.")

(defconst discourse-auth--scopes "write,session_info"
  "Least User API Key scopes needed for authenticated reading and writing.")

(defconst discourse-auth--page-script
  "(async () => { const node = document.querySelector('#user-api-key-payload'); if (!node) return null; const response = await fetch('/session/current.json', { headers: { Accept: 'application/json' } }); if (!response.ok) return null; const data = await response.json(); const user = data.current_user; if (!user || !user.id || !user.username) return null; return { payload: node.textContent.replace(/\\s/g, ''), user_id: String(user.id), username: user.username }; })()"
  "Provider script that captures the encrypted result and public identity.")

(cl-defstruct (discourse-auth-result
               (:constructor discourse-auth-result-create)
               (:copier nil))
  ok-p
  account
  message)

(cl-defstruct (discourse-auth-request
               (:constructor discourse-auth-request--create)
               (:copier nil))
  active-p
  browser-request
  capture-file
  private-key-file
  nonce
  client-id
  origin
  owner
  handle
  callback)

(defun discourse-auth--owner-live-p (owner)
  "Return non-nil when Appkit OWNER remains live."
  (or (appkit-app-live-p owner) (appkit-surface-live-p owner)))

(defun discourse-auth--ensure-directory ()
  "Return the private authorization directory after securing it."
  (let ((directory (file-name-as-directory
                    (expand-file-name discourse-auth-directory))))
    (when (file-symlink-p directory)
      (user-error "Discourse auth directory must not be a symbolic link"))
    (make-directory directory t)
    (set-file-modes directory #o700)
    directory))

(defun discourse-auth--openssl ()
  "Return the OpenSSL executable or signal a user error."
  (or (executable-find "openssl")
      (user-error "OpenSSL is required for Discourse User API Key authorization")))

(defun discourse-auth--openssl-output (&rest arguments)
  "Run OpenSSL ARGUMENTS and return its unibyte standard output."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary)
          (status
           (apply #'call-process
                  (discourse-auth--openssl) nil t nil arguments)))
      (unless (and (integerp status) (zerop status))
        (error "OpenSSL failed while processing Discourse authorization"))
      (buffer-string))))

(defun discourse-auth--private-key-file (origin)
  "Return ORIGIN's stable RSA private-key file, creating it atomically."
  (let* ((directory (discourse-auth--ensure-directory))
         (digest (secure-hash 'sha256 origin))
         (file (expand-file-name (format "client-%s.pem" digest) directory)))
    (cond
     ((file-symlink-p file)
      (user-error "Discourse RSA private key must not be a symbolic link"))
     ((file-exists-p file)
      (unless (file-regular-p file)
        (user-error "Discourse RSA private key is not a regular file"))
      (set-file-modes file #o600))
     (t
      (let ((temporary (make-temp-file
                        (expand-file-name ".client-key-" directory))))
        (unwind-protect
            (progn
              (let ((status
                     (call-process
                      (discourse-auth--openssl) nil nil nil
                      "genpkey" "-algorithm" "RSA"
                      "-pkeyopt" "rsa_keygen_bits:2048"
                      "-out" temporary)))
                (unless (and (integerp status) (zerop status))
                  (error "OpenSSL could not generate a Discourse RSA key")))
              (set-file-modes temporary #o600)
              (rename-file temporary file nil))
          (when (file-exists-p temporary)
            (delete-file temporary))))))
    file))

(defun discourse-auth--public-key (private-key-file)
  "Return the PEM public key belonging to PRIVATE-KEY-FILE."
  (decode-coding-string
   (discourse-auth--openssl-output
    "pkey" "-in" private-key-file "-pubout")
   'utf-8-unix))

(defun discourse-auth--nonce ()
  "Return a cryptographically random authorization nonce."
  (string-trim
   (decode-coding-string
    (discourse-auth--openssl-output "rand" "-hex" "32")
    'us-ascii)))

(defun discourse-auth--client-id (public-key)
  "Return a stable installation client ID derived from PUBLIC-KEY."
  (concat "emacs-discourse-"
          (substring (secure-hash 'sha256 public-key) 0 24)))

(defun discourse-auth--encode-query (parameters)
  "Encode string PARAMETERS as an application query."
  (mapconcat
   (lambda (entry)
     (concat (url-hexify-string (car entry))
             "="
             (url-hexify-string (cdr entry))))
   parameters
   "&"))

(defun discourse-auth--authorization-url (origin public-key client-id nonce)
  "Return ORIGIN authorization URL for PUBLIC-KEY, CLIENT-ID, and NONCE."
  (unless (and (stringp discourse-auth-application-name)
               (not (string-empty-p discourse-auth-application-name)))
    (user-error "Discourse authorization application name must not be empty"))
  (concat
   origin
   "/user-api-key/new?"
   (discourse-auth--encode-query
    `(("application_name" . ,discourse-auth-application-name)
      ("client_id" . ,client-id)
      ("nonce" . ,nonce)
      ("scopes" . ,discourse-auth--scopes)
      ("public_key" . ,public-key)
      ("padding" . "oaep")))))

(defun discourse-auth--valid-api-key-p (value)
  "Return non-nil when VALUE is a bounded single-line API key."
  (and (stringp value)
       (<= 16 (length value) 256)
       (string-match-p "\\`[[:graph:]]+\\'" value)
       (not (string-match-p "[\r\n]" value))))

(defun discourse-auth--decrypt-payload (payload private-key-file nonce)
  "Decrypt base64 PAYLOAD with PRIVATE-KEY-FILE and verify NONCE."
  (unless (and (stringp payload) (<= (length payload) 16384))
    (error "Discourse authorization payload is invalid"))
  (let ((cipher-file (make-temp-file "discourse-auth-payload-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file cipher-file
              (set-buffer-multibyte nil)
              (insert
               (condition-case nil
                   (base64-decode-string payload)
                 (error
                  (error "Discourse authorization payload is not valid base64"))))))
          (set-file-modes cipher-file #o600)
          (let* ((plain
                  (decode-coding-string
                   (discourse-auth--openssl-output
                    "pkeyutl" "-decrypt"
                    "-inkey" private-key-file
                    "-pkeyopt" "rsa_padding_mode:oaep"
                    "-in" cipher-file)
                   'utf-8-unix))
                 (data
                  (condition-case error-data
                      (json-parse-string
                       plain
                       :object-type 'hash-table
                       :array-type 'list
                       :null-object nil
                       :false-object :json-false)
                    (json-parse-error
                     (error "Invalid decrypted Discourse authorization: %s"
                            (error-message-string error-data)))))
                 (returned-nonce (and (hash-table-p data)
                                      (gethash "nonce" data)))
                 (key (and (hash-table-p data) (gethash "key" data))))
            (unless (equal returned-nonce nonce)
              (error "Discourse authorization nonce does not match"))
            (unless (discourse-auth--valid-api-key-p key)
              (error "Discourse returned an invalid User API Key"))
            (substring-no-properties key)))
      (when (file-exists-p cipher-file)
        (delete-file cipher-file)))))

(defun discourse-auth--source-host (origin)
  "Return the auth-source machine name for normalized ORIGIN.
HTTPS is the only supported scheme, so the host (plus a non-default port)
identifies the origin without putting a URL in the netrc machine field."
  (let* ((normalized (discourse-runtime-normalize-origin origin))
         (parsed (url-generic-parse-url normalized))
         (host (url-host parsed))
         (port (url-port parsed)))
    (concat host
            (if (= port 443) "" (format ":%d" port)))))

(defun discourse-auth--source-spec (origin username)
  "Return canonical auth-source identity spec for ORIGIN and USERNAME."
  (list :host (discourse-auth--source-host origin)
        :user username
        :port discourse-auth--source-port))

(defun discourse-auth--token-value (token key)
  "Return TOKEN's scalar KEY value without text properties."
  (let ((value (plist-get token key)))
    (and (stringp value) (substring-no-properties value))))

(defun discourse-auth--source-tokens (origin &optional username)
  "Return validated auth-source tokens for ORIGIN and optional USERNAME."
  (let ((tokens
         (apply
          #'auth-source-search
          (append
           (list :host (discourse-auth--source-host origin)
                 :port discourse-auth--source-port
                 :max 100
                 :require '(:secret :client-id :user-id))
           (and username (list :user username))))))
    (cl-remove-if-not
     (lambda (token)
       (condition-case nil
           (and (discourse-auth--token-value token :user)
                (discourse-auth--token-value token :client-id)
                (discourse-state-id
                 (discourse-auth--token-value token :user-id)))
         (error nil)))
     tokens)))

(defun discourse-auth--matching-token (account)
  "Return ACCOUNT's exact auth-source token, or nil."
  (cl-find-if
   (lambda (token)
     (and (equal (discourse-account-client-id account)
                 (discourse-auth--token-value token :client-id))
          (equal (discourse-account-user-id account)
                 (discourse-auth--token-value token :user-id))))
   (discourse-auth--source-tokens
    (discourse-account-origin account)
    (discourse-account-username account))))

(defun discourse-auth-api-key (account)
  "Return ACCOUNT's User API Key from auth-source.
Anonymous accounts return nil."
  (when (discourse-account-authenticated-p account)
    (let* ((token (discourse-auth--matching-token account))
           (key (and token (auth-info-password token))))
      (unless (discourse-auth--valid-api-key-p key)
        (signal 'discourse-auth-credential-error
                (list (format "No valid User API Key is stored for %s"
                              (discourse-account-display-name account)))))
      (substring-no-properties key))))

(defun discourse-auth--store-api-key
    (origin user-id username client-id api-key)
  "Replace ORIGIN USERNAME's stored API-KEY and identity metadata."
  (let ((spec (discourse-auth--source-spec origin username))
        ;; A credential must never enter auth-source diagnostics.
        (auth-source-debug nil)
        (auth-source-creation-defaults `((secret . ,api-key))))
    (when (discourse-auth--source-tokens origin username)
      (apply #'auth-source-delete spec)
      (auth-source-forget-all-cached))
    (let* ((created
            (apply
             #'auth-source-search
             (append
              spec
              (list :client-id client-id
                    :user-id user-id
                    :max 1
                    :require '(:secret :client-id :user-id)
                    :create '(client-id user-id)))))
           (save-function (and created
                               (plist-get (car created) :save-function))))
      (when save-function
        (funcall save-function))
      (auth-source-forget-all-cached)
      (let* ((account
              (discourse-account--create
               :origin origin
               :identity 'user-api-key
               :user-id user-id
               :username username
               :client-id client-id))
             (token (discourse-auth--matching-token account))
             (stored (and token (auth-info-password token))))
        (unless (equal stored api-key)
          (error "Discourse User API Key was not saved by auth-source"))))))

(defun discourse-auth--account-from-token (origin token)
  "Create ORIGIN's authenticated account described by auth-source TOKEN."
  (discourse-runtime-create-authenticated-account
   origin
   (discourse-auth--token-value token :user-id)
   (discourse-auth--token-value token :user)
   (discourse-auth--token-value token :client-id)))

(defun discourse-auth-connect (origin &optional username)
  "Return a stored authenticated account for ORIGIN and optional USERNAME.
When several identities exist and USERNAME is nil, ask which one to use."
  (setq origin (discourse-runtime-normalize-origin origin))
  (let* ((tokens (discourse-auth--source-tokens origin username))
         (token
          (pcase tokens
            ('nil nil)
            (`(,only) only)
            (_
             (let* ((choices
                     (mapcar
                      (lambda (candidate)
                        (cons
                         (format
                          "%s · %s"
                          (discourse-auth--token-value candidate :user)
                          (discourse-auth--token-value candidate :user-id))
                         candidate))
                      tokens))
                    (choice
                     (completing-read
                      "Discourse identity: " choices nil t)))
               (cdr (assoc choice choices)))))))
    (and token (discourse-auth--account-from-token origin token))))

(defun discourse-auth--capture-data (file)
  "Return validated private authorization data from capture FILE."
  (let* ((capture (browser-session-read file))
         (page (browser-session-page capture))
         (payload (and (listp page) (alist-get 'payload page)))
         (user-id (and (listp page) (alist-get 'user_id page)))
         (username (and (listp page) (alist-get 'username page))))
    (unless (and (stringp payload)
                 (stringp username)
                 (not (string-empty-p username)))
      (error "Browser did not return a complete Discourse authorization"))
    (list :payload payload
          :user-id (discourse-state-id user-id)
          :username (substring-no-properties username))))

(defun discourse-auth--request-current-p (request)
  "Return non-nil when authorization REQUEST may still settle."
  (and (discourse-auth-request-active-p request)
       (discourse-auth--owner-live-p
        (discourse-auth-request-owner request))))

(defun discourse-auth--delete-capture (request)
  "Delete REQUEST's private browser capture if present."
  (when-let* ((file (discourse-auth-request-capture-file request)))
    (when (file-exists-p file)
      (delete-file file))))

(defun discourse-auth--retire (request)
  "Retire REQUEST without invoking its cancellation side effect."
  (when (discourse-auth-request-active-p request)
    (setf (discourse-auth-request-active-p request) nil
          (discourse-auth-request-browser-request request) nil)
    (when-let* ((handle (discourse-auth-request-handle request)))
      (when (appkit-handle-alive-p handle)
        (appkit-retire-handle handle)))
    (discourse-auth--delete-capture request)))

(defun discourse-auth--emit (request result)
  "Settle current authorization REQUEST with RESULT exactly once."
  (when (discourse-auth--request-current-p request)
    (let ((callback (discourse-auth-request-callback request)))
      (discourse-auth--retire request)
      (funcall callback result))))

(defun discourse-auth--cancel-owned (request)
  "Cancel lifecycle-owned authorization REQUEST."
  (when (discourse-auth-request-active-p request)
    (setf (discourse-auth-request-active-p request) nil)
    (when-let* ((browser-request
                 (discourse-auth-request-browser-request request)))
      (when (browser-session-request-live-p browser-request)
        (browser-session-cancel browser-request)))
    (discourse-auth--delete-capture request)))

(defun discourse-auth-cancel (request)
  "Cancel asynchronous authorization REQUEST."
  (unless (discourse-auth-request-p request)
    (user-error "Invalid Discourse authorization request"))
  (when (discourse-auth-request-active-p request)
    (discourse-auth--cancel-owned request)
    (when-let* ((handle (discourse-auth-request-handle request)))
      (when (appkit-handle-alive-p handle)
        (appkit-retire-handle handle)))
    t))

(defun discourse-auth--accept-capture (request)
  "Decrypt, store, and accept browser authorization REQUEST."
  (condition-case error-data
      (let* ((captured
              (discourse-auth--capture-data
               (discourse-auth-request-capture-file request)))
             (origin (discourse-auth-request-origin request))
             (user-id (plist-get captured :user-id))
             (username (plist-get captured :username))
             (client-id (discourse-auth-request-client-id request))
             (api-key
              (discourse-auth--decrypt-payload
               (plist-get captured :payload)
               (discourse-auth-request-private-key-file request)
               (discourse-auth-request-nonce request))))
        (discourse-auth--store-api-key
         origin user-id username client-id api-key)
        (discourse-auth--emit
         request
         (discourse-auth-result-create
          :ok-p t
          :account
          (discourse-runtime-create-authenticated-account
           origin user-id username client-id))))
    (error
     (discourse-auth--emit
      request
      (discourse-auth-result-create
       :ok-p nil
       :message (error-message-string error-data))))))

(cl-defun discourse-auth-authorize (origin callback &key owner)
  "Authorize a User API Key for ORIGIN and report it to CALLBACK.

The browser session is used only to log in and capture the RSA-encrypted result;
no browser cookie is read into Emacs.  OWNER defaults to ORIGIN's anonymous
application and owns cancellation.  Return a cancellable authorization request."
  (unless (functionp callback)
    (error "Discourse authorization callback must be callable"))
  (setq origin (discourse-runtime-normalize-origin origin))
  (let* ((anonymous (discourse-runtime-create-account origin))
         (owner (or owner (discourse-account-app anonymous)))
         (private-key-file (discourse-auth--private-key-file origin))
         (public-key (discourse-auth--public-key private-key-file))
         (nonce (discourse-auth--nonce))
         (client-id (discourse-auth--client-id public-key))
         (capture-file
          (make-temp-name
           (expand-file-name "authorization-"
                             (discourse-auth--ensure-directory))))
         (request
           (discourse-auth-request--create
            :active-p t
            :capture-file capture-file
            :private-key-file private-key-file
            :nonce nonce
            :client-id client-id
            :origin origin
            :owner owner
            :callback callback))
         browser-request
         handle)
    (unless (discourse-auth--owner-live-p owner)
      (error "Cannot authorize through a stopped Appkit owner"))
    (condition-case error-data
        (progn
          (setq browser-request
                (browser-session-capture
                 :url
                 (discourse-auth--authorization-url
                  origin public-key client-id nonce)
                 :output-file capture-file
                 :profile-root
                 (expand-file-name "browser-session/"
                                   (discourse-auth--ensure-directory))
                 :script discourse-auth--page-script
                 :callback
                 (lambda (_metadata)
                   (when (discourse-auth--request-current-p request)
                     (discourse-auth--accept-capture request)))
                 :errorback
                 (lambda (error-object)
                   (discourse-auth--emit
                    request
                    (discourse-auth-result-create
                     :ok-p nil
                     :message
                     (browser-session-error-message error-object))))))
          (setf (discourse-auth-request-browser-request request)
                browser-request)
          (setq handle
                (appkit-register-handle
                 owner 'discourse-authorization request
                 #'discourse-auth--cancel-owned))
          (setf (discourse-auth-request-handle request) handle)
          request)
      (error
       (setf (discourse-auth-request-active-p request) nil)
       (discourse-auth--delete-capture request)
       (signal (car error-data) (cdr error-data))))))

(provide 'discourse-auth)

;;; discourse-auth.el ends here
