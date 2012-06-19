;;; org-e-iorg-js.el -- interactive HTML export

;;; Code:
(require 'org-export)
(require 'org-element)
(require 'org-e-html)
(require 'iorg-util)

(defvar org-e-iorg-js-style
  (concat
   "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n"
   (file-contents (expand-file-name "org-e-iorg-js.css" iorg-base))
   "/*]]>*/-->\n</style>"))

(defvar org-e-iorg-js-jquery
  "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")

(defvar org-e-iorg-js-scripts
  (concat
   "<script type=\"text/javascript\" src=\"" org-e-iorg-js-jquery "\"></script>"
   "<script type=\"text/javascript\">\n<!--/*--><![CDATA[/*><!--*/\n"
   (file-contents (expand-file-name "org-e-iorg-js.js" iorg-base))
   "/*]]>*///-->\n</script>\n"))

(defvar org-e-iorg-js-wrap-template
  (concat "<div class=\"edit_in_place\">%html-text</div>"
          "<div class=\"raw-org\">%org-text</div>"))

(defun org-e-iorg-js-interactive-p (element info)
  (let ((parent (org-export-get-parent element)))
    (cond ((eq (car parent) 'headline)
           (member "iorg" (org-export-get-tags parent info)))
          ((eq (car parent) 'org-data) nil)
          ((member (car parent) '(paragraph plain-list)) nil)
          (t (org-e-iorg-js-interactive-p parent info)))))

(defmacro def-e-ihtml-wrap (e-html-function)
  "Defines and returns an e-ihtml-wrapped version of E-HTML-FUNCTION."
  (let ((fname (intern (concat "org-e-iorg-js"
                               (substring (symbol-name e-html-function) 10)))))
    `(defun ,fname (element contents info)
       ,(format "e-ihtml wrapper around `%s'." e-html-function)
       (let* ((original-contents (copy-seq contents))
              (original-info     (copy-seq info))
              (html-text (,e-html-function element contents info))
              (org-text  (or (org-element-interpret-data element)
                             original-contents
                             "NIL")))
         (if (org-e-iorg-js-interactive-p element info)
             (org-fill-template org-e-iorg-js-wrap-template
                                (list (cons "html-text" html-text)
                                      (cons "org-text"  org-text)))
           html-text)))))

(eval `(org-export-define-derived-backend e-ihtml e-html
         :translate-alist
         ((paragraph  . ,(def-e-ihtml-wrap org-e-html-paragraph))
          (plain-list . ,(def-e-ihtml-wrap org-e-html-plain-list)))))

(defun org-e-iorg-js-export-to-html
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to an interactive HTML file."
  (interactive)
  (let* ((extension (concat "." org-e-html-extension))
	 (file (org-export-output-file-name extension subtreep pub-dir))
	 (org-export-coding-system org-e-html-coding-system)
         ;; custom headers
         (org-e-html-style-extra org-e-iorg-js-style)
         (org-e-html-scripts org-e-iorg-js-scripts))
    (org-export-to-file 'e-ihtml
                        file subtreep visible-only body-only ext-plist)))

(defun org-e-iorg-js-export-file (file)
  "Export FILE's contents to interactive HTML."
  (save-window-excursion
    (find-file file)
    (org-e-iorg-js-export-to-html)))

(provide 'org-e-iorg-js)
;;; org-e-iorg-js.el ends here
