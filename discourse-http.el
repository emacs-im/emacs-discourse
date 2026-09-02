;;; discourse-http.el --- Strict asynchronous Discourse transport -*- lexical-binding: t; -*-

;;; Commentary:

;; Anonymous JSON reads with lifecycle cancellation, bounded 429 Retry-After
;; handling, response limits, and one typed completion path.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'time-date)
(require 'url-util)
(require 'plz)
(require 'appkit-core)
(require 'discourse-customize)
(require 'discourse-runtime)

(defconst discourse-http-user-agent "discourse.el/0.1.0"
  "User-Agent header sent by discourse.el.")

(cl-defstruct (discourse-http-failure
               (:constructor discourse-http-failure-create)
               (:copier nil))
  kind
  status
  message
  retry-after)

(cl-defstruct (discourse-http-result
               (:constructor discourse-http-result-create)
               (:copier nil))
  ok-p
  status
  headers
  data
  failure)

(cl-defstruct (discourse-http-request
               (:constructor discourse-http-request--create)
               (:copier nil))
  account
  owner
  endpoint
  parameters
  attempt
  process
  timer
  handle
  callback
  active-p)

(defun discourse-http--owner-live-p (owner)
  "Return non-nil when Appkit OWNER is live."
  (or (appkit-app-live-p owner) (appkit-view-live-p owner)))

(defun discourse-http--endpoint-url (account endpoint parameters)
  "Return validated ACCOUNT URL for ENDPOINT and PARAMETERS."
  (unless (and (stringp endpoint)
               (string-prefix-p "/" endpoint)
               (not (string-prefix-p "//" endpoint))
               (not (string-match-p "[\r\n#]" endpoint)))
    (error "Invalid Discourse API endpoint"))
  (let ((query (discourse-http-encode-parameters parameters)))
    (concat (discourse-account-origin account)
            endpoint
            (if (string-empty-p query)
                ""
              (concat (if (string-match-p "?" endpoint) "&" "?") query)))))

(defun discourse-http--parameter-value (value)
  "Return URL representation of parameter VALUE."
  (cond
   ((stringp value) value)
   ((integerp value) (number-to-string value))
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   ((eq value :json-false) "false")
   ((symbolp value) (symbol-name value))
   ((null value) "")
   (t (error "Invalid Discourse query parameter value"))))

(defun discourse-http-encode-parameters (parameters)
  "Encode query PARAMETERS, preserving repeated keys."
  (mapconcat
   (lambda (entry)
     (unless (consp entry)
       (error "Invalid Discourse query parameter"))
     (concat
      (url-hexify-string (format "%s" (car entry)))
      "="
      (url-hexify-string
       (discourse-http--parameter-value (cdr entry)))))
   parameters
   "&"))

(defun discourse-http--parse-json (body)
  "Parse JSON BODY as hash tables and vectors."
  (json-parse-string
   body
   :object-type 'hash-table
   :array-type 'array
   :null-object nil
   :false-object :json-false))

(defun discourse-http--header (response name)
  "Return RESPONSE header NAME case-insensitively."
  (let ((target (downcase name)))
    (cl-loop for (key . value) in (plz-response-headers response)
             when (equal target (downcase (format "%s" key)))
             return value)))

(defun discourse-http--retry-after (response)
  "Return bounded Retry-After seconds from RESPONSE, or nil."
  (when-let* ((value (discourse-http--header response "retry-after"))
              (text (string-trim (format "%s" value))))
    (let ((seconds
           (if (string-match-p "\\`[0-9]+\\'" text)
               (string-to-number text)
             (condition-case nil
                 (max 0 (- (float-time (date-to-time text)) (float-time)))
               (error nil)))))
      (and (numberp seconds)
           (> seconds 0)
           (min seconds 3600)))))

(defun discourse-http--json-message (data fallback)
  "Return a bounded server message from JSON DATA or FALLBACK."
  (let* ((errors
          (and (hash-table-p data)
               (gethash "errors" data)))
         (errors
          (cond
           ((vectorp errors) (append errors nil))
           ((proper-list-p errors) errors)
           (t nil)))
         (message
          (or (and errors
                   (string-join
                    (cl-loop for value in errors
                             when (stringp value)
                             collect value)
                    "; "))
              (and (hash-table-p data)
                   (or (gethash "message" data)
                       (gethash "error" data)))
              fallback)))
    (if (stringp message)
        (truncate-string-to-width
         (replace-regexp-in-string "[\r\n]+" " " message)
         300 nil nil "…")
      fallback)))

(defun discourse-http--decode-response (response)
  "Return parsed JSON from RESPONSE or signal a bounded content error."
  (let ((body (or (plz-response-body response) "")))
    (when (> (string-bytes body) discourse-http-response-byte-limit)
      (error "Discourse response exceeds configured byte limit"))
    (when (string-empty-p body)
      (error "Discourse returned an empty JSON response"))
    (discourse-http--parse-json body)))

(defun discourse-http--request-current-p (request)
  "Return non-nil when REQUEST may still publish."
  (and (discourse-http-request-active-p request)
       (discourse-http--owner-live-p
        (discourse-http-request-owner request))
       (appkit-app-live-p
        (discourse-account-app (discourse-http-request-account request)))
       (let ((handle (discourse-http-request-handle request)))
         (and (appkit-handle-p handle) (appkit-handle-alive-p handle)))))

(defun discourse-http--cancel-owned-request (request)
  "Cancel process and timer resources owned by REQUEST."
  (setf (discourse-http-request-active-p request) nil)
  (when-let* ((timer (discourse-http-request-timer request)))
    (when (timerp timer) (cancel-timer timer)))
  (when-let* ((process (discourse-http-request-process request)))
    (when (and (processp process) (process-live-p process))
      (set-process-filter process nil)
      (set-process-sentinel process nil)
      (delete-process process)))
  (setf (discourse-http-request-timer request) nil
        (discourse-http-request-process request) nil))

(defun discourse-http--retire-request (request)
  "Retire REQUEST without invoking cancellation callbacks."
  (when (discourse-http-request-active-p request)
    (setf (discourse-http-request-active-p request) nil
          (discourse-http-request-timer request) nil
          (discourse-http-request-process request) nil)
    (when-let* ((handle (discourse-http-request-handle request)))
      (when (appkit-handle-alive-p handle)
        (appkit-retire-handle handle)))))

(defun discourse-http--emit (request result)
  "Settle current REQUEST exactly once with RESULT."
  (when (discourse-http--request-current-p request)
    (let ((callback (discourse-http-request-callback request)))
      (discourse-http--retire-request request)
      (funcall callback result))))

(defun discourse-http--failure-result
    (kind status message &optional retry-after headers)
  "Return failed result for KIND, STATUS, MESSAGE, and RETRY-AFTER."
  (discourse-http-result-create
   :ok-p nil
   :status status
   :headers headers
   :failure
   (discourse-http-failure-create
    :kind kind
    :status status
    :message message
    :retry-after retry-after)))

(defun discourse-http--schedule-retry (request delay)
  "Schedule REQUEST after server-provided DELAY seconds."
  (when (discourse-http--request-current-p request)
    (setf
     (discourse-http-request-timer request)
     (run-at-time
      delay nil
      (lambda ()
        (if (discourse-http--request-current-p request)
            (progn
              (setf (discourse-http-request-timer request) nil)
              (discourse-http--dispatch request))
          (discourse-http--retire-request request)))))))

(defun discourse-http--handle-success (request response)
  "Handle successful plz RESPONSE for REQUEST."
  (when (discourse-http--request-current-p request)
    (condition-case error-data
        (discourse-http--emit
         request
         (discourse-http-result-create
          :ok-p t
          :status (plz-response-status response)
          :headers (plz-response-headers response)
          :data (discourse-http--decode-response response)))
      (error
       (discourse-http--emit
        request
        (discourse-http--failure-result
         'invalid-response
         (plz-response-status response)
         (error-message-string error-data)
         nil
         (plz-response-headers response)))))))

(defun discourse-http--error-response (error-object)
  "Return HTTP response embedded in plz ERROR-OBJECT, or nil."
  (and (plz-error-p error-object)
       (ignore-errors (plz-error-response error-object))))

(defun discourse-http--handle-failure (request error-object)
  "Handle HTTP or transport ERROR-OBJECT for REQUEST."
  (when (discourse-http--request-current-p request)
    (let* ((response (discourse-http--error-response error-object))
           (status (or (and response (plz-response-status response)) 0))
           (headers (and response (plz-response-headers response)))
           (retry-after (and response
                             (= status 429)
                             (discourse-http--retry-after response))))
      (if (and retry-after
               (< (discourse-http-request-attempt request)
                  discourse-read-retry-limit))
          (progn
            (cl-incf (discourse-http-request-attempt request))
            (setf (discourse-http-request-process request) nil)
            (discourse-http--schedule-retry request retry-after))
        (let ((data
               (and response
                    (condition-case nil
                        (discourse-http--decode-response response)
                      (error nil)))))
          (discourse-http--emit
           request
           (discourse-http--failure-result
            (if response 'http 'transport)
            status
            (if response
                (discourse-http--json-message
                 data (format "Discourse request failed with HTTP %d" status))
              "Discourse transport request failed")
            retry-after
            headers)))))))

(defun discourse-http--dispatch (request)
  "Dispatch one attempt for current REQUEST."
  (when (discourse-http--request-current-p request)
    (let* ((account (discourse-http-request-account request))
           (url
            (discourse-http--endpoint-url
             account
             (discourse-http-request-endpoint request)
             (discourse-http-request-parameters request)))
           ;; Following redirects could silently cross the configured origin.
           (plz-curl-default-args
            (remove "--location" plz-curl-default-args))
           (process
            (plz 'get url
              :headers `(("Accept" . "application/json")
                         ("User-Agent" . ,discourse-http-user-agent))
              :body-type 'text
              :as 'response
              :timeout discourse-http-timeout
              :connect-timeout discourse-http-timeout
              :noquery t
              :then (lambda (response)
                      (discourse-http--handle-success request response))
              :else (lambda (error-object)
                      (discourse-http--handle-failure request error-object)))))
      (if (discourse-http--request-current-p request)
          (setf (discourse-http-request-process request) process)
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(cl-defun discourse-http-get
    (account endpoint callback &key parameters owner)
  "Issue an anonymous JSON GET for ACCOUNT ENDPOINT.

CALLBACK receives one `discourse-http-result'.  OWNER defaults to ACCOUNT's
Appkit application and owns process/timer cancellation."
  (unless (and (discourse-account-p account)
               (appkit-app-live-p (discourse-account-app account)))
    (error "Cannot request through a dead Discourse account"))
  (unless (functionp callback)
    (error "Discourse HTTP callback must be callable"))
  (let* ((effective-owner (or owner (discourse-account-app account)))
         (request
          (discourse-http-request--create
           :account account
           :owner effective-owner
           :endpoint endpoint
           :parameters (copy-tree parameters)
           :attempt 0
           :callback callback
           :active-p t))
         (handle
          (appkit-register-handle
           effective-owner 'discourse-http request
           #'discourse-http--cancel-owned-request)))
    (setf (discourse-http-request-handle request) handle)
    (condition-case error-data
        (discourse-http--dispatch request)
      (error
       (discourse-http--emit
        request
        (discourse-http--failure-result
         'transport 0 (error-message-string error-data)))))
    request))

(defun discourse-http-cancel (request)
  "Cancel live Discourse HTTP REQUEST without publishing a result."
  (when (and (discourse-http-request-p request)
             (discourse-http-request-active-p request))
    (if-let* ((handle (discourse-http-request-handle request)))
        (appkit-cancel-handle handle)
      (discourse-http--cancel-owned-request request))
    t))

(provide 'discourse-http)

;;; discourse-http.el ends here
