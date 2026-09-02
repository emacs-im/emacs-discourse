;;; discourse-markup.el --- Discourse cooked HTML to Appkit markup -*- lexical-binding: t; -*-

;;; Commentary:

;; Direct, bounded adaptation of authoritative Discourse cooked HTML into
;; immutable Appkit semantic documents.  This module parses HTML with libxml but
;; never delegates presentation to SHR.  Provider-only structures remain opaque
;; objects with complete visible fallbacks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-expand)
(require 'appkit-markup)
(require 'discourse-customize)

(defconst discourse-markup--discard-tags
  '(script style head iframe frame frameset object embed form input textarea
           select option button video audio source track canvas svg link meta
           base template noscript)
  "HTML elements whose complete subtrees are not post content.")

(defconst discourse-markup--block-tags
  '(address article aside blockquote details div dl fieldset figure figcaption
            footer h1 h2 h3 h4 h5 h6 header hr li main nav ol p pre section
            table ul)
  "Elements establishing block boundaries in cooked HTML.")

(cl-defstruct (discourse-markup-provider-object
               (:constructor discourse-markup-provider-object-create)
               (:copier nil))
  kind
  data)

(defvar discourse-markup--node-count 0)
(defvar discourse-markup--object-count 0)
(defvar discourse-markup--context nil)

(defun discourse-markup-libxml-available-p ()
  "Return non-nil when this Emacs can parse HTML with libxml."
  (and (fboundp 'libxml-parse-html-region)
       (or (not (fboundp 'libxml-available-p))
           (libxml-available-p))))

(defun discourse-markup--tick (depth)
  "Account for one DOM node at DEPTH or reject oversized input."
  (cl-incf discourse-markup--node-count)
  (when (> discourse-markup--node-count discourse-markup-node-limit)
    (error "Discourse cooked markup exceeds the node limit"))
  (when (> depth discourse-markup-depth-limit)
    (error "Discourse cooked markup exceeds the nesting limit")))

(defun discourse-markup--element-p (node)
  "Return non-nil when NODE is a libxml element."
  (and (consp node) (symbolp (car node))))

(defun discourse-markup--attributes (node)
  "Return DOM NODE's attribute alist."
  (let ((candidate (and (discourse-markup--element-p node) (cadr node))))
    (if (and (listp candidate) (cl-every #'consp candidate)) candidate nil)))

(defun discourse-markup--children (node)
  "Return DOM NODE's children."
  (if (discourse-markup--attributes node) (cddr node) (cdr node)))

(defun discourse-markup--attribute (node name)
  "Return DOM NODE attribute NAME."
  (cdr (assq name (discourse-markup--attributes node))))

(defun discourse-markup--classes (node)
  "Return DOM NODE class names."
  (split-string
   (or (discourse-markup--attribute node 'class) "")
   "[[:space:]]+" t))

(defun discourse-markup--class-p (node class)
  "Return non-nil when DOM NODE carries CLASS."
  (member class (discourse-markup--classes node)))

(defun discourse-markup--clean-string (value)
  "Return VALUE as a property-free string."
  (substring-no-properties (format "%s" (or value ""))))

(defun discourse-markup--collapsed-text (text)
  "Return cooked TEXT with HTML layout whitespace collapsed."
  (replace-regexp-in-string
   "[[:space:]\r\n]+" " " (discourse-markup--clean-string text)))

(defun discourse-markup--text-content (node &optional depth)
  "Return recursive literal text content of DOM NODE at DEPTH."
  (setq depth (or depth 0))
  (discourse-markup--tick depth)
  (cond
   ((stringp node) (discourse-markup--clean-string node))
   ((not (discourse-markup--element-p node)) "")
   ((memq (car node) discourse-markup--discard-tags) "")
   ((eq (car node) 'br) "\n")
   (t
    (mapconcat
     (lambda (child)
       (discourse-markup--text-content child (1+ depth)))
     (discourse-markup--children node) ""))))

(defun discourse-markup--safe-url (url base-url)
  "Return URL resolved against BASE-URL when its scheme is safe."
  (when (stringp url)
    (let ((url (string-trim (substring-no-properties url)))
          (case-fold-search t))
      (cond
       ((string-empty-p url) nil)
       ((string-match-p "\\`\\(?:https?\\|mailto\\):" url) url)
       ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url) nil)
       ((and (stringp base-url) (not (string-empty-p base-url)))
        (url-expand-file-name url (file-name-as-directory base-url)))
       (t url)))))

(defun discourse-markup--object-data (kind data)
  "Return owned provider DATA decorated for KIND and current context."
  (cl-incf discourse-markup--object-count)
  (append
   (list :key
         (list (plist-get discourse-markup--context :post-id)
               kind discourse-markup--object-count)
         :context (copy-tree discourse-markup--context))
   (copy-tree data)))

(defun discourse-markup--object (kind data fallback &optional styles)
  "Return inline provider object KIND with DATA, FALLBACK, and STYLES."
  (appkit-markup-object
   (discourse-markup-provider-object-create
    :kind kind :data (discourse-markup--object-data kind data))
   fallback styles))

(defun discourse-markup--object-block (kind data fallback)
  "Return provider object block KIND with DATA and FALLBACK blocks."
  (appkit-markup-object-block
   (discourse-markup-provider-object-create
    :kind kind :data (discourse-markup--object-data kind data))
   fallback))

(defun discourse-markup--add-styles (nodes styles)
  "Return inline NODES with semantic STYLES appended."
  (mapcar
   (lambda (node)
     (cond
      ((appkit-markup-text-p node)
       (appkit-markup-text
        (appkit-markup-text-text node)
        (append (appkit-markup-text-styles node) styles)))
      ((appkit-markup-object-p node)
       (appkit-markup-object
        (appkit-markup-object-value node)
        (appkit-markup-object-fallback node)
        (append (appkit-markup-object-styles node) styles)))
      (t node)))
   nodes))

(defun discourse-markup--inline-fallback-text (nodes)
  "Flatten inline NODES to styled text suitable for a link label."
  (let (result)
    (dolist (node nodes)
      (cond
       ((appkit-markup-text-p node) (push node result))
       ((appkit-markup-line-break-p node)
        (push (appkit-markup-text " ") result))
       ((appkit-markup-link-p node)
        (setq result
              (nconc
               (nreverse
                (copy-sequence (appkit-markup-link-children node)))
               result)))
       ((appkit-markup-object-p node)
        (setq result
              (nconc
               (nreverse
                (discourse-markup--inline-fallback-text
                 (appkit-markup-object-fallback node)))
               result)))))
    (nreverse result)))

(defun discourse-markup--find-descendant (node predicate &optional depth)
  "Return first NODE descendant satisfying PREDICATE."
  (setq depth (or depth 0))
  (discourse-markup--tick depth)
  (when (discourse-markup--element-p node)
    (or (and (funcall predicate node) node)
        (cl-loop for child in (discourse-markup--children node)
                 when (discourse-markup--element-p child)
                 thereis
                 (discourse-markup--find-descendant
                  child predicate (1+ depth))))))

(defun discourse-markup--inline-children (children base-url depth)
  "Adapt DOM CHILDREN to Appkit inlines using BASE-URL at DEPTH."
  (let (result)
    (dolist (child children result)
      (setq result
            (nconc result
                   (discourse-markup--inline-node
                    child base-url depth))))))

(defun discourse-markup--image-object (node base-url)
  "Return inline semantic image object for DOM NODE."
  (let* ((emoji-p (discourse-markup--class-p node "emoji"))
         (alt
          (discourse-markup--clean-string
           (or (discourse-markup--attribute node 'alt)
               (discourse-markup--attribute node 'title)
               (if emoji-p ":emoji:" "[image]"))))
         (src
          (discourse-markup--safe-url
           (or (discourse-markup--attribute node 'data-original-src)
               (discourse-markup--attribute node 'src))
           base-url)))
    (discourse-markup--object
     (if emoji-p 'emoji 'image)
     (list :url src :alt alt
           :name (discourse-markup--clean-string
                  (discourse-markup--attribute node 'title)))
     (list (appkit-markup-text alt)))))

(defun discourse-markup--anchor-node (node base-url depth)
  "Adapt anchor DOM NODE using BASE-URL at DEPTH."
  (let* ((children
          (discourse-markup--inline-children
           (discourse-markup--children node) base-url (1+ depth)))
         (label (discourse-markup--inline-fallback-text children))
         (url
          (discourse-markup--safe-url
           (discourse-markup--attribute node 'href) base-url))
         (image
          (discourse-markup--find-descendant
           node (lambda (child) (eq (car child) 'img))))
         (classes (discourse-markup--classes node)))
    (cond
     ((and image (not (discourse-markup--class-p image "emoji")))
      (let ((alt
             (discourse-markup--clean-string
              (or (discourse-markup--attribute image 'alt) "image")))
            (preview
             (discourse-markup--safe-url
              (discourse-markup--attribute image 'src) base-url)))
        (list
         (discourse-markup--object
          'media
          (list :url (or url preview) :preview-url preview :alt alt)
          (list (appkit-markup-text (format "[Image: %s]" alt)))))))
     ((or (member "mention" classes) (member "mention-group" classes))
      (list
       (discourse-markup--object
        (if (member "mention-group" classes) 'group-mention 'mention)
        (list :url url
              :username
              (discourse-markup--clean-string
               (or (discourse-markup--attribute node 'data-username)
                   (discourse-markup--text-content node (1+ depth)))))
        label)))
     ((member "footnote-backref" classes)
      (list
       (discourse-markup--object
        'footnote-backref (list :url url) label)))
     ((and (member "anchor" classes) (null label)) nil)
     ((and url label) (list (appkit-markup-link url label)))
     (t label))))

(defun discourse-markup--inline-spoiler (node base-url depth)
  "Return inline spoiler object for DOM NODE."
  (let ((content
         (discourse-markup--inline-children
          (discourse-markup--children node) base-url (1+ depth))))
    (list
     (discourse-markup--object
      'spoiler
      (list :content
            (appkit-markup-document
             (list (appkit-markup-paragraph content))))
      (append
       (list (appkit-markup-text "[spoiler: "))
       content
       (list (appkit-markup-text "]")))))))

(defun discourse-markup--inline-node (node base-url depth)
  "Adapt one DOM NODE to zero or more inline nodes at DEPTH."
  (discourse-markup--tick depth)
  (cond
   ((stringp node)
    (let ((text (discourse-markup--collapsed-text node)))
      (unless (string-empty-p text)
        (list (appkit-markup-text text)))))
   ((not (discourse-markup--element-p node)) nil)
   ((memq (car node) discourse-markup--discard-tags) nil)
   ((eq (car node) 'br) (list (appkit-markup-line-break)))
   ((eq (car node) 'img)
    (list (discourse-markup--image-object node base-url)))
   ((eq (car node) 'a)
    (discourse-markup--anchor-node node base-url depth))
   ((and (eq (car node) 'sup)
         (discourse-markup--class-p node "footnote-ref"))
    (let* ((anchor
            (discourse-markup--find-descendant
             node (lambda (child) (eq (car child) 'a))))
           (fallback
            (discourse-markup--inline-children
             (discourse-markup--children node) base-url (1+ depth))))
      (list
       (discourse-markup--object
        'footnote-reference
        (list :url
              (discourse-markup--safe-url
               (and anchor (discourse-markup--attribute anchor 'href))
               base-url))
        fallback))))
   ((and (eq (car node) 'span)
         (discourse-markup--class-p node "discourse-local-date"))
    (let ((fallback
           (discourse-markup--inline-children
            (discourse-markup--children node) base-url (1+ depth))))
      (list
       (discourse-markup--object
        'local-date
        (list :date (discourse-markup--attribute node 'data-date)
              :time (discourse-markup--attribute node 'data-time)
              :timezone (discourse-markup--attribute node 'data-timezone)
              :email-preview
              (discourse-markup--attribute node 'data-email-preview))
        fallback))))
   ((and (memq (car node) '(span div))
         (or (discourse-markup--class-p node "math")
             (discourse-markup--class-p node "asciimath")))
    (let* ((kind (if (discourse-markup--class-p node "asciimath")
                     'asciimath
                   'math))
           (source (string-trim
                    (discourse-markup--text-content node (1+ depth)))))
      (list
       (discourse-markup--object
        kind (list :source source)
        (list (appkit-markup-text source '(code)))))))
   ((and (eq (car node) 'span)
         (discourse-markup--class-p node "chcklst-box"))
    (let ((checked (discourse-markup--class-p node "checked")))
      (list
       (discourse-markup--object
        'checklist (list :checked-p checked)
        (list (appkit-markup-text (if checked "☑" "☐")))))))
   ((and (memq (car node) '(span div))
         (or (discourse-markup--class-p node "spoiler")
             (discourse-markup--class-p node "spoiled")))
    (discourse-markup--inline-spoiler node base-url depth))
   (t
    (let* ((children
            (discourse-markup--inline-children
             (discourse-markup--children node) base-url (1+ depth)))
           (styles
            (pcase (car node)
              ((or 'strong 'b) '(bold))
              ((or 'em 'i) '(italic))
              ('u '(underline))
              ((or 's 'strike 'del) '(strike))
              ('code '(code))
              (_ nil))))
      (cond
       ((memq (car node) '(kbd mark))
        (list
         (discourse-markup--object
          (if (eq (car node) 'kbd) 'keyboard 'mark)
          nil children
          (and (eq (car node) 'kbd) '(code)))))
       (styles (discourse-markup--add-styles children styles))
       (t children))))))

(defun discourse-markup--list-block (node base-url depth)
  "Return semantic list block for DOM NODE at DEPTH."
  (let (items)
    (dolist (child (discourse-markup--children node))
      (when (and (discourse-markup--element-p child)
                 (eq (car child) 'li))
        (push
         (appkit-markup-list-item
          (discourse-markup--blocks
           (discourse-markup--children child) base-url (1+ depth)))
         items)))
    (let ((start-value (discourse-markup--attribute node 'start)))
      (list
       (appkit-markup-list
        (if (eq (car node) 'ol) 'ordered 'unordered)
        (nreverse items)
        :start
        (and (eq (car node) 'ol)
             (stringp start-value)
             (string-match-p "\\`[1-9][0-9]*\\'" start-value)
             (string-to-number start-value)))))))

(defun discourse-markup--code-language (node)
  "Return opaque code language label represented by NODE."
  (or (discourse-markup--attribute node 'data-code-language)
      (cl-loop for class in (discourse-markup--classes node)
               when (string-match
                     "\\`\\(?:lang\\|language\\)-\\(.+\\)\\'" class)
               return (match-string 1 class))))

(defun discourse-markup--quote-block (node base-url depth)
  "Return provider-aware quote object for aside NODE."
  (let* ((blockquote
          (discourse-markup--find-descendant
           node (lambda (child) (eq (car child) 'blockquote))))
         (title
          (seq-find
           (lambda (child)
             (and (discourse-markup--element-p child)
                  (discourse-markup--class-p child "title")))
           (discourse-markup--children node)))
         (content
          (if blockquote
              (discourse-markup--blocks
               (discourse-markup--children blockquote)
               base-url (1+ depth))
            nil))
         (title-text
          (and title
               (string-trim
                (discourse-markup--text-content title (1+ depth)))))
         (fallback
          (append
           (and title-text
                (not (string-empty-p title-text))
                (list
                 (appkit-markup-paragraph
                  (list (appkit-markup-text title-text '(bold))))))
           (and content (list (appkit-markup-quote content))))))
    (list
     (discourse-markup--object-block
      'quote
      (list :username (discourse-markup--attribute node 'data-username)
            :topic-id (discourse-markup--attribute node 'data-topic)
            :post-number (discourse-markup--attribute node 'data-post))
      fallback))))

(defun discourse-markup--onebox-block (node base-url depth)
  "Return provider onebox block for DOM NODE."
  (let* ((article
          (discourse-markup--find-descendant
           node (lambda (child) (eq (car child) 'article))))
         (anchor
          (discourse-markup--find-descendant
           node (lambda (child) (eq (car child) 'a))))
         (url
          (discourse-markup--safe-url
           (or (discourse-markup--attribute node 'data-onebox-src)
               (and anchor (discourse-markup--attribute anchor 'href)))
           base-url))
         (fallback
          (discourse-markup--blocks
           (discourse-markup--children (or article node))
           base-url (1+ depth))))
    (list
     (discourse-markup--object-block
      'onebox (list :url url) fallback))))

(defun discourse-markup--details-block (node base-url depth)
  "Return semantic details object block for DOM NODE."
  (let* ((summary
          (seq-find
           (lambda (child)
             (and (discourse-markup--element-p child)
                  (eq (car child) 'summary)))
           (discourse-markup--children node)))
         (content-nodes
          (cl-remove summary (discourse-markup--children node) :test #'eq))
         (summary-inlines
          (and summary
               (discourse-markup--inline-children
                (discourse-markup--children summary)
                base-url (1+ depth))))
         (content
          (discourse-markup--blocks content-nodes base-url (1+ depth)))
         (summary-document
          (appkit-markup-document
           (list
            (appkit-markup-paragraph
             (or summary-inlines
                 (list (appkit-markup-text "Details")))))))
         (content-document (appkit-markup-document content)))
    (list
     (discourse-markup--object-block
      'details
      (list :summary summary-document :content content-document
            :open-p (not (null (assq 'open
                                     (discourse-markup--attributes node)))))
      (append
       (appkit-markup-document-blocks summary-document)
       content)))))

(defun discourse-markup--spoiler-block (node base-url depth)
  "Return block spoiler object for DOM NODE."
  (let* ((content
          (discourse-markup--blocks
           (discourse-markup--children node) base-url (1+ depth)))
         (document (appkit-markup-document content)))
    (list
     (discourse-markup--object-block
      'spoiler (list :content document)
      (append
       (list
        (appkit-markup-paragraph
         (list (appkit-markup-text "Spoiler" '(bold)))))
       content)))))

(defun discourse-markup--poll-block (node base-url depth)
  "Return poll object block for DOM NODE."
  (let* ((container
          (discourse-markup--find-descendant
           node
           (lambda (child)
             (discourse-markup--class-p child "poll-container"))))
         (fallback
          (discourse-markup--blocks
           (discourse-markup--children (or container node))
           base-url (1+ depth))))
    (list
     (discourse-markup--object-block
      'poll
      (list :name (discourse-markup--attribute node 'data-poll-name)
            :status (discourse-markup--attribute node 'data-poll-status)
            :type (discourse-markup--attribute node 'data-poll-type))
      fallback))))

(defun discourse-markup--table-text (node depth)
  "Return conservative tab-separated text for table NODE at DEPTH."
  (let (rows)
    (cl-labels
        ((walk
          (candidate current-depth)
          (discourse-markup--tick current-depth)
          (when (discourse-markup--element-p candidate)
            (if (eq (car candidate) 'tr)
                (let (cells)
                  (dolist (child (discourse-markup--children candidate))
                    (when (and (discourse-markup--element-p child)
                               (memq (car child) '(th td)))
                      (push
                       (string-trim
                        (discourse-markup--text-content
                         child (1+ current-depth)))
                       cells)))
                  (when cells
                    (push (string-join (nreverse cells) "\t") rows)))
              (dolist (child (discourse-markup--children candidate))
                (when (discourse-markup--element-p child)
                  (walk child (1+ current-depth))))))))
      (walk node depth))
    (string-join (nreverse rows) "\n")))

(defun discourse-markup--lazy-video-block (node base-url depth)
  "Return lazy-video provider block for DOM NODE."
  (ignore depth)
  (let* ((anchor
          (discourse-markup--find-descendant
           node (lambda (child) (eq (car child) 'a))))
         (url
          (discourse-markup--safe-url
           (and anchor (discourse-markup--attribute anchor 'href)) base-url))
         (title
          (discourse-markup--clean-string
           (or (discourse-markup--attribute node 'data-video-title)
               "Video")))
         (label (list (appkit-markup-text (format "[Video: %s]" title))))
         (fallback
          (list
           (appkit-markup-paragraph
            (if url (list (appkit-markup-link url label)) label)))))
    (list
     (discourse-markup--object-block
      'lazy-video (list :url url :title title) fallback))))

(defun discourse-markup--block-node (node base-url depth)
  "Adapt block DOM NODE to zero or more Appkit blocks at DEPTH."
  (let ((tag (car node)))
    (cond
     ((memq tag discourse-markup--discard-tags) nil)
     ((memq tag '(p address figcaption))
      (list
       (appkit-markup-paragraph
        (discourse-markup--inline-children
         (discourse-markup--children node) base-url (1+ depth)))))
     ((memq tag '(h1 h2 h3 h4 h5 h6))
      (list
       (appkit-markup-heading
        (string-to-number (substring (symbol-name tag) 1))
        (discourse-markup--inline-children
         (discourse-markup--children node) base-url (1+ depth)))))
     ((and (eq tag 'aside) (discourse-markup--class-p node "quote"))
      (discourse-markup--quote-block node base-url depth))
     ((and (eq tag 'div)
           (discourse-markup--class-p node "lazy-video-container"))
      (discourse-markup--lazy-video-block node base-url depth))
     ((and (memq tag '(aside div))
           (discourse-markup--class-p node "onebox"))
      (discourse-markup--onebox-block node base-url depth))
     ((eq tag 'blockquote)
      (list
       (appkit-markup-quote
        (discourse-markup--blocks
         (discourse-markup--children node) base-url (1+ depth)))))
     ((memq tag '(ul ol))
      (discourse-markup--list-block node base-url depth))
     ((eq tag 'pre)
      (let* ((code
              (discourse-markup--find-descendant
               node (lambda (child) (eq (car child) 'code))))
             (language
              (or (discourse-markup--code-language node)
                  (and code (discourse-markup--code-language code)))))
        (list
         (appkit-markup-preformatted
          (discourse-markup--text-content (or code node) (1+ depth))
          language))))
     ((eq tag 'details)
      (discourse-markup--details-block node base-url depth))
     ((and (eq tag 'div)
           (or (discourse-markup--class-p node "spoiler")
               (discourse-markup--class-p node "spoiled")))
      (discourse-markup--spoiler-block node base-url depth))
     ((and (eq tag 'div) (discourse-markup--class-p node "poll"))
      (discourse-markup--poll-block node base-url depth))
     ((and (eq tag 'div)
           (or (discourse-markup--class-p node "math")
               (discourse-markup--class-p node "asciimath")))
      (let* ((kind (if (discourse-markup--class-p node "asciimath")
                       'asciimath
                     'math))
             (source
              (string-trim
               (discourse-markup--text-content node (1+ depth)))))
        (list
         (discourse-markup--object-block
          kind (list :source source)
          (list (appkit-markup-preformatted source kind))))))
     ((eq tag 'table)
      (list
       (discourse-markup--object-block
        'table nil
        (list
         (appkit-markup-preformatted
          (discourse-markup--table-text node (1+ depth)))))))
     ((eq tag 'hr)
      (list
       (discourse-markup--object-block
        'thematic-break nil
        (list
         (appkit-markup-paragraph
          (list (appkit-markup-text "────────────────")))))))
     (t
      (discourse-markup--blocks
       (discourse-markup--children node) base-url (1+ depth))))))

(defun discourse-markup--blocks (nodes base-url depth)
  "Adapt DOM NODES to Appkit blocks using BASE-URL at DEPTH."
  (let (blocks pending-inline)
    (cl-labels
        ((flush-inline
          ()
          (when pending-inline
            (push (appkit-markup-paragraph pending-inline) blocks)
            (setq pending-inline nil))))
      (dolist (node nodes)
        (discourse-markup--tick depth)
        (cond
         ((and (stringp node)
               (string-match-p "\\`[[:space:]\r\n]*\\'" node)) nil)
         ((and (discourse-markup--element-p node)
               (memq (car node) discourse-markup--block-tags))
          (flush-inline)
          (setq blocks
                (nconc
                 (nreverse
                  (discourse-markup--block-node node base-url depth))
                 blocks)))
         (t
          (setq pending-inline
                (nconc
                 pending-inline
                 (discourse-markup--inline-node node base-url depth))))))
      (flush-inline))
    (nreverse blocks)))

(defun discourse-markup--parse-html (html)
  "Parse HTML into a stable wrapped libxml DOM."
  (with-temp-buffer
    (insert "<!doctype html><html><body><div id=\"discourse-appkit-root\">")
    (insert html)
    (insert "</div></body></html>")
    (libxml-parse-html-region (point-min) (point-max))))

(defun discourse-markup--root (document)
  "Return private wrapper element from parsed DOCUMENT."
  (discourse-markup--find-descendant
   document
   (lambda (node)
     (equal (discourse-markup--attribute node 'id)
            "discourse-appkit-root"))))

(defun discourse-markup--decode-numeric-entities (text)
  "Decode valid numeric entities in fallback TEXT."
  (let ((start 0))
    (while (string-match
            "&#\\(?:\\([xX]\\)\\([0-9A-Fa-f]+\\)\\|\\([0-9]+\\)\\);"
            text start)
      (let* ((hex-p (match-string 1 text))
             (digits (or (match-string 2 text) (match-string 3 text)))
             (number (string-to-number digits (if hex-p 16 10)))
             (replacement
              (if (or (= number 0)
                      (> number #x10ffff)
                      (and (<= #xd800 number) (<= number #xdfff)))
                  "�"
                (if (and (< number 32) (not (memq number '(9 10 13))))
                    "�"
                  (char-to-string number)))))
        (setq text (replace-match replacement t t text)
              start (+ (match-beginning 0) (length replacement)))))
    text))

(defun discourse-markup--decode-common-entities (text)
  "Decode conservative common entities in fallback TEXT."
  (dolist (mapping
           '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<")
             ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'")
             ("&apos;" . "'") ("&hellip;" . "…")
             ("&mdash;" . "—") ("&ndash;" . "–"))
           text)
    (setq text
          (replace-regexp-in-string
           (regexp-quote (car mapping)) (cdr mapping) text t t))))

(defun discourse-markup--fallback-text (html)
  "Return conservative plain text when libxml cannot parse HTML."
  (let ((text (or html ""))
        (case-fold-search t))
    (setq text
          (replace-regexp-in-string
           "<!--\\(?:.\\|\n\\)*?-->" "" text t t))
    (dolist (tag discourse-markup--discard-tags)
      (setq text
            (replace-regexp-in-string
             (format
              "<%s\\(?:[[:space:]][^>]*\\)?>\\(?:.\\|\n\\)*?</%s[[:space:]]*>"
              tag tag)
             "" text t t)))
    (setq text
          (replace-regexp-in-string
           "<br\\(?:[[:space:]][^>]*\\)?/?>" "\n" text t t)
          text
          (replace-regexp-in-string
           "<li\\(?:[[:space:]][^>]*\\)?>" "- " text t t))
    (dolist (tag discourse-markup--block-tags)
      (setq text
            (replace-regexp-in-string
             (format "</%s[[:space:]]*>" tag) "\n" text t t)))
    (setq text (replace-regexp-in-string "<[^>]*>" "" text t t)
          text (discourse-markup--decode-numeric-entities text)
          text (discourse-markup--decode-common-entities text)
          text (replace-regexp-in-string "\r" "" text t t)
          text
          (replace-regexp-in-string
           "\n[[:space:]\n]*\n" "\n\n" text))
    (string-trim text)))

(defun discourse-markup--plain-document (text)
  "Return semantic document for fallback plain TEXT."
  (appkit-markup-document
   (mapcar
    (lambda (line)
      (appkit-markup-paragraph
       (list (appkit-markup-text line))))
    (split-string text "\n" t))))

(cl-defun discourse-markup-parse (html &optional base-url &key context)
  "Return Appkit document adapted from cooked HTML.

Resolve relative links against BASE-URL.  CONTEXT is copied into provider
objects.  Active DOM subtrees are discarded.  Missing libxml and parse failures
produce conservative semantic plain text, never a second HTML renderer."
  (setq html (discourse-markup--clean-string html))
  (when (> (length html) discourse-markup-source-limit)
    (error "Discourse cooked markup exceeds the source limit"))
  (let ((discourse-markup--node-count 0)
        (discourse-markup--object-count 0)
        (discourse-markup--context (copy-tree context)))
    (condition-case nil
        (if (not (discourse-markup-libxml-available-p))
            (discourse-markup--plain-document
             (discourse-markup--fallback-text html))
          (let* ((dom (discourse-markup--parse-html html))
                 (root (discourse-markup--root dom)))
            (unless root
              (error "Discourse cooked wrapper was not parsed"))
            (appkit-markup-document
             (discourse-markup--blocks
              (discourse-markup--children root) base-url 0))))
      (error
       (discourse-markup--plain-document
        (discourse-markup--fallback-text html))))))

(defun discourse-markup-plain-text (html &optional base-url)
  "Return semantic plain text for cooked HTML and BASE-URL."
  (appkit-markup-plain-text
   (discourse-markup-parse html base-url)))

(provide 'discourse-markup)

;;; discourse-markup.el ends here
