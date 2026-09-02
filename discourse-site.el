;;; discourse-site.el --- Shared public Discourse site metadata -*- lexical-binding: t; -*-

;;; Commentary:

;; Lazily populate account-owned site identity and category state for any view
;; that needs it.  Requests remain owned by the requesting Appkit view; the
;; canonical observations live on the account.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)
(require 'discourse-api)
(require 'discourse-http)
(require 'discourse-runtime)
(require 'discourse-state)

(defun discourse-site--failure-message (result)
  "Return a concise message for failed metadata RESULT."
  (let ((failure (discourse-http-result-failure result)))
    (if (discourse-http-failure-p failure)
        (discourse-http-failure-message failure)
      "unknown response failure")))

(defun discourse-site--report-failure (kind result)
  "Report failed public metadata RESULT for KIND."
  (message "Discourse %s metadata unavailable: %s"
           kind (discourse-site--failure-message result)))

(cl-defun discourse-site-ensure-metadata (account callback &key owner)
  "Ensure ACCOUNT's public site metadata and notify CALLBACK.

CALLBACK receives `categories' or `profile' after that observation is
installed.  Already cached observations need no notification.  OWNER defaults
to ACCOUNT's app and owns any asynchronous requests.  Return the requests that
were started."
  (unless (and (discourse-account-p account)
               (appkit-app-live-p (discourse-account-app account)))
    (error "Cannot load metadata for a stopped Discourse account"))
  (unless (functionp callback)
    (error "Discourse site metadata callback is not callable"))
  (let* ((state (discourse-account-state account))
         (owner (or owner (discourse-account-app account)))
         requests)
    (unless (discourse-state-categories-loaded-p state)
      (when-let* ((request
                   (discourse-api-site-categories
                    account
                    (lambda (result)
                      (if (discourse-http-result-ok-p result)
                          (condition-case error-data
                              (progn
                                (discourse-state-merge-categories
                                 state (discourse-http-result-data result))
                                (funcall callback 'categories))
                            (error
                             (message
                              "Invalid Discourse category metadata: %s"
                              (error-message-string error-data))))
                        (discourse-site--report-failure "category" result)))
                    :owner owner)))
        (push request requests)))
    (unless (discourse-state-site-profile state)
      (when-let* ((request
                   (discourse-api-site-profile
                    account
                    (lambda (result)
                      (if (discourse-http-result-ok-p result)
                          (condition-case error-data
                              (progn
                                (discourse-state-set-site-profile
                                 state (discourse-http-result-data result))
                                (funcall callback 'profile))
                            (error
                             (message
                              "Invalid Discourse site profile: %s"
                              (error-message-string error-data))))
                        (discourse-site--report-failure "site" result)))
                    :owner owner)))
        (push request requests)))
    (nreverse requests)))

(provide 'discourse-site)

;;; discourse-site.el ends here
