;;; discourse-markup-test.el --- Markup contracts for discourse.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'appkit-markup-ui)
(require 'discourse-markup)

(defun discourse-markup-test--collect-provider-kinds (document)
  "Return provider object kinds recursively present in DOCUMENT."
  (let (kinds)
    (cl-labels
        ((walk-inlines
          (nodes)
          (dolist (node nodes)
            (cond
             ((appkit-markup-object-p node)
              (let ((value (appkit-markup-object-value node)))
                (when (discourse-markup-provider-object-p value)
                  (push (discourse-markup-provider-object-kind value) kinds)))
              (walk-inlines (appkit-markup-object-fallback node)))
             ((appkit-markup-link-p node)
              (walk-inlines (appkit-markup-link-children node))))))
         (walk-blocks
          (blocks)
          (dolist (block blocks)
            (cond
             ((appkit-markup-paragraph-p block)
              (walk-inlines (appkit-markup-paragraph-children block)))
             ((appkit-markup-heading-p block)
              (walk-inlines (appkit-markup-heading-children block)))
             ((appkit-markup-quote-p block)
              (walk-blocks (appkit-markup-quote-blocks block)))
             ((appkit-markup-list-p block)
              (dolist (item (appkit-markup-list-items block))
                (walk-blocks (appkit-markup-list-item-blocks item))))
             ((appkit-markup-object-block-p block)
              (let ((value (appkit-markup-object-block-value block)))
                (when (discourse-markup-provider-object-p value)
                  (push (discourse-markup-provider-object-kind value) kinds)))
              (walk-blocks (appkit-markup-object-block-fallback block)))))))
      (walk-blocks (appkit-markup-document-blocks document)))
    (nreverse kinds)))

(ert-deftest discourse-markup-adapts-common-cooked-html-natively ()
  (let ((document
         (discourse-markup-parse
          (concat
           "<p>Hello <strong>bold</strong> "
           "<a href=\"/t/topic/42\">topic</a></p>"
           "<ul><li>one</li><li>two</li></ul>")
          "https://example.test")))
    (should (equal "Hello bold topic\n- one\n- two"
                   (appkit-markup-plain-text document)))
    (with-temp-buffer
      (appkit-markup-ui-insert-document
       document :final-newline-p nil :interactive-p t
       :link-action (lambda (_url) #'ignore))
      (should (equal "Hello bold topic\none\ntwo"
                     (substring-no-properties (buffer-string))))
      (goto-char (point-min))
      (search-forward "one")
      (should (equal "• " (get-text-property (1- (point)) 'line-prefix)))
      (goto-char (point-min))
      (search-forward "bold")
      (should (memq 'bold
                    (let ((face (get-text-property (1- (point)) 'face)))
                      (if (listp face) face (list face))))))))

(ert-deftest discourse-markup-discards-active-content-and-unsafe-links ()
  (let* ((document
          (discourse-markup-parse
           (concat
            "<p>safe<script>secret()</script>"
            "<a href=\"javascript:alert(1)\">bad</a></p>")
           "https://example.test"))
         (blocks (appkit-markup-document-blocks document))
         (children (appkit-markup-paragraph-children (car blocks))))
    (should (equal "safebad" (appkit-markup-plain-text document)))
    (should-not (seq-some #'appkit-markup-link-p children))))

(ert-deftest discourse-markup-preserves-discourse-extensions-as-objects ()
  (let* ((html
          (concat
           "<p><a class=\"mention\" href=\"/u/ada\">@ada</a> "
           "<span class=\"discourse-local-date\" data-date=\"2026-09-01\">date</span> "
           "<span class=\"math\">x^2</span>"
           "<span class=\"chcklst-box checked\"></span></p>"
           "<details><summary>More</summary><p>Body</p></details>"
           "<div class=\"poll\" data-poll-name=\"poll\">"
           "<div class=\"poll-container\"><ul><li>A</li><li>B</li></ul></div></div>"
           "<aside class=\"quote\" data-username=\"ada\" data-post=\"1\" data-topic=\"42\">"
           "<div class=\"title\">Ada:</div><blockquote><p>Quoted</p></blockquote></aside>"))
         (document
          (discourse-markup-parse html "https://example.test"
                                  :context '(:post-id "91")))
         (kinds (discourse-markup-test--collect-provider-kinds document)))
    (dolist (kind '(mention local-date math checklist details poll quote))
      (should (memq kind kinds)))
    (should (string-match-p "Body" (appkit-markup-plain-text document)))
    (should (string-match-p "Quoted" (appkit-markup-plain-text document)))))

(ert-deftest discourse-markup-promotes-lightboxes-to-media-card-objects ()
  (let* ((document
          (discourse-markup-parse
           (concat
            "<div class=\"lightbox-wrapper\">"
            "<a class=\"lightbox\" href=\"/original.png\" title=\"Shot\">"
            "<img src=\"/preview.png\" alt=\"Screenshot\""
            " width=\"660\" height=\"500\">"
            "<span class=\"informations\">1152×872 111 KB</span>"
            "</a></div>")
           "https://example.test"
           :context '(:post-id "91")))
         (block (car (appkit-markup-document-blocks document)))
         (value (and (appkit-markup-object-block-p block)
                     (appkit-markup-object-block-value block)))
         (data (and value
                    (discourse-markup-provider-object-data value))))
    (should (appkit-markup-object-block-p block))
    (should (eq 'media
                (discourse-markup-provider-object-kind value)))
    (should (equal "https://example.test/original.png"
                   (plist-get data :url)))
    (should (equal "https://example.test/preview.png"
                   (plist-get data :preview-url)))
    (should (equal "660" (plist-get data :width)))
    (should (equal "500" (plist-get data :height)))
    (should (equal "1152×872 111 KB"
                   (plist-get data :information)))
    (should (equal '(:post-id "91")
                   (plist-get data :context)))))

(ert-deftest discourse-markup-preserves-rich-onebox-metadata ()
  (let* ((document
          (discourse-markup-parse
           (concat
            "<aside class=\"onebox githubrepo\""
            " data-onebox-src=\"https://github.com/a/repo\">"
            "<header class=\"source\"><a href=\"https://github.com/a/repo\">"
            "github.com</a></header>"
            "<article><img class=\"thumbnail\" src=\"https://img.test/repo\""
            " width=\"690\" height=\"344\">"
            "<h3>GitHub - a/repo</h3>"
            "<p><span class=\"github-repo-description\">A useful package</span></p>"
            "</article></aside>")
           "https://example.test"
           :context '(:post-id "91")))
         (block (car (appkit-markup-document-blocks document)))
         (value (appkit-markup-object-block-value block))
         (data (discourse-markup-provider-object-data value)))
    (should (eq 'onebox
                (discourse-markup-provider-object-kind value)))
    (should (equal "https://github.com/a/repo"
                   (plist-get data :url)))
    (should (equal "github.com" (plist-get data :provider)))
    (should (equal "GitHub - a/repo" (plist-get data :title)))
    (should (equal "A useful package"
                   (plist-get data :description)))
    (should (equal "https://img.test/repo"
                   (plist-get data :image-url)))
    (should (equal "githubrepo"
                   (plist-get data :onebox-kind)))))

(ert-deftest discourse-markup-has-conservative-no-libxml-fallback ()
  (cl-letf (((symbol-function 'discourse-markup-libxml-available-p)
             (lambda () nil)))
    (should
     (equal "Hello\nworld"
            (discourse-markup-plain-text
             "<p>Hello<script>secret</script></p><p>world</p>")))))

(provide 'discourse-markup-test)

;;; discourse-markup-test.el ends here
