;;; discourse-customize.el --- User options for discourse.el -*- lexical-binding: t; -*-

;;; Commentary:

;; User-visible instance, transport, paging, and rendering policy.

;;; Code:

(defgroup discourse nil
  "Appkit-based Discourse client."
  :group 'applications
  :prefix "discourse-")

(defcustom discourse-default-origin "https://emacs-china.org"
  "HTTPS origin opened by `discourse'."
  :type 'string
  :group 'discourse)

(defcustom discourse-http-timeout 30
  "Maximum seconds for one ordinary Discourse HTTP attempt."
  :type 'number
  :group 'discourse)

(defcustom discourse-http-response-byte-limit (* 8 1024 1024)
  "Maximum accepted JSON response size in bytes."
  :type 'integer
  :group 'discourse)

(defcustom discourse-read-retry-limit 1
  "Maximum automatic retries for an idempotent read after HTTP 429."
  :type 'integer
  :group 'discourse)

(defcustom discourse-topic-post-page-size 20
  "Maximum post IDs requested by one topic pagination read."
  :type 'integer
  :group 'discourse)

(defcustom discourse-markup-source-limit (* 1024 1024)
  "Maximum cooked HTML characters accepted for one post."
  :type 'integer
  :group 'discourse)

(defcustom discourse-markup-node-limit 20000
  "Maximum DOM nodes accepted while adapting one cooked post."
  :type 'integer
  :group 'discourse)

(defcustom discourse-markup-depth-limit 64
  "Maximum DOM nesting accepted while adapting one cooked post."
  :type 'integer
  :group 'discourse)

(provide 'discourse-customize)

;;; discourse-customize.el ends here
