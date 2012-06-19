;;; iorg-js.el -- interactive HTML export with JavaScript

;;; Code:
(require 'org-export)
(require 'org-element)
(require 'org-e-html)
(require 'iorg-util)

(defvar iorg-js-style
  (concat
   "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n"
   (file-contents (expand-file-name "iorg-js.css" iorg-base))
   "/*]]>*/-->\n</style>"))

(defvar iorg-js-jquery
  "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")

(defvar iorg-js-scripts
  (concat
   "<script type=\"text/javascript\" src=\"" iorg-js-jquery "\"></script>"
   "<script type=\"text/javascript\">\n<!--/*--><![CDATA[/*><!--*/\n"
   (file-contents (expand-file-name "iorg-js.js" iorg-base))
   "/*]]>*///-->\n</script>\n"))

(defvar iorg-js-wrap-template
  (concat "<div class=\"edit_in_place\">%html-text</div>"
          "<div class=\"raw-org\">%org-text</div>"))

(defun iorg-js-interactive-p (element info)
  (let ((parent (org-export-get-parent element)))
    (cond ((eq (car parent) 'headline)
           (member "iorg" (org-export-get-tags parent info)))
          ((eq (car parent) 'org-data) nil)
          ((member (car parent) '(paragraph plain-list)) nil)
          (t (iorg-js-interactive-p parent info)))))

(defmacro def-iorg-js-wrap (e-html-function)
  "Defines and returns an iorg-js-wrapped version of E-HTML-FUNCTION."
  (let ((fname (intern (concat "iorg-js"
                               (substring (symbol-name e-html-function) 10)))))
    `(defun ,fname (element contents info)
       ,(format "iorg-js wrapper around `%s'." e-html-function)
       (let* ((original-contents (copy-seq contents))
              (original-info     (copy-seq info))
              (html-text (,e-html-function element contents info))
              (org-text  (or (org-element-interpret-data element)
                             original-contents
                             "NIL")))
         (if (iorg-js-interactive-p element info)
             (org-fill-template iorg-js-wrap-template
                                (list (cons "html-text" html-text)
                                      (cons "org-text"  org-text)))
           html-text)))))

(eval `(org-export-define-derived-backend iorg-js e-html
         :translate-alist
         ((paragraph  . ,(def-iorg-js-wrap html-paragraph))
          (plain-list . ,(def-iorg-js-wrap html-plain-list)))))

(defun iorg-js-export-to-html
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to an interactive HTML file."
  (interactive)
  (let* ((extension (concat "." html-extension))
	 (file (org-export-output-file-name extension subtreep pub-dir))
	 (org-export-coding-system html-coding-system)
         ;; custom headers
         (html-style-extra iorg-js-style)
         (html-scripts iorg-js-scripts))
    (org-export-to-file
     'iorg-js file subtreep visible-only body-only ext-plist)))

(defun iorg-js-export-file (file)
  "Export FILE's contents to interactive HTML."
  (save-window-excursion
    (find-file file)
    (iorg-js-export-to-html)))

(provide 'iorg-js)
;;; iorg-js.el ends here
