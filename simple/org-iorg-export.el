;;; org-iorg-export -- back-end for exporting Org files to interactive HTML

(require 'org-export)
(require 'org-element)
(require 'org-e-html)
;; (require 'org-iorg)


;;; Define derived backend
;; (org-export-define-derived-backend iorg e-html
;;   :translate-alist ((headline . org-iorg-headline)
;;                     (item . org-iorg-item)
;;                     (paragraph . org-iorg-paragraph)
;;                     (plain-list . org-iorg-plain-list)
;;                     (section . org-iorg-section)))
(defvar iorg-wrap-template
  (concat
   "<div id=...>%html-text</div>"
   "<!-- html/js to make this editable by clicking a button -->"
   "<div style=\"display:none\"><form>%org-text</form></div>"))

(defmacro def-iorg-wrap (e-html-function)
  "Defines and returns an iorg-wrapped version of E-HTML-FUNCTION."
  (let ((fname (intern (concat "iorg"
                               (substring (symbol-name 'org-e-html-item) 3)))))
    `(defun ,fname (element contents info)
       ,(format "iOrg wrapper around `%s'." e-html-function)
       (let* ((original-contents (copy-seq contents))
              (original-info     (copy-seq info))
              (html-text (,e-html-function element contents info))
              (org-text  (or (org-element-interpret-data element)
                             original-contents
                             "NIL")))
         (if (not (member "iorg" (org-export-get-tags element original-info)))
             html-text
           (org-fill-template iorg-wrap-template
                              (list (cons "html-text" html-text)
                                    (cons "org-text"  org-text))))))))

;; A couple of interesting things are happening here.  We work around
;; a bug in `org-export-define-derived-backend' in which it will not
;; change the value of an already defined translation alist (because
;; it uses `defvar').  To work around this we simply define the alist
;; ourselves calculating it's value in the same manner as does
;; `org-export-define-derived-backend'.  And we explicitly check if
;; the alist is already defined and if so we use `setq' instead of
;; `defvar'.
;;
;; In setting the value of this alist we use the `def-iorg-wrap' macro
;; to define translation functions and return their symbol names for
;; insertion into the `org-iorg-translate-alist'.
(let* ((fmt-str      "org-%s-translate-alist")
       (child-alist  (intern (format fmt-str 'iorg)))
       (parent-alist (intern (format fmt-str 'e-html))))
  (eval (list (if (boundp child-alist) #'setq #'defvar) child-alist
              `(quote
                ,(append
                  `((headline   . org-iorg-headline)
                    (item       . ,(def-iorg-wrap org-e-html-item))
                    (paragraph  . ,(def-iorg-wrap org-e-html-paragraph))
                    (section    . org-iorg-section))
                  (copy-sequence (symbol-value parent-alist)))))))


;;; Customisation group
(defgroup org-iorg-export nil
  "Options for exporting Org files to dynamic html."
  :tag "Org iOrg Export"
  :group 'org-iorg)


;;; Headline
;; Headline customization variables
(defcustom org-iorg-format-headline-function nil
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).

The function result will be used in the section format string.

As an example, one could set the variable to the following, in
order to reproduce the default set-up:

\(defun org-e-html-format-headline \(todo todo-type priority text tags)
  \"Default format function for an headline.\"
  \(concat \(when todo
            \(format \"\\\\textbf{\\\\textsc{\\\\textsf{%s}}} \" todo))
	  \(when priority
            \(format \"\\\\framebox{\\\\#%c} \" priority))
	  text
	  \(when tags (format \"\\\\hfill{}\\\\textsc{%s}\" tags))))"
  :group 'org-iorg-export
  :type 'function)


;; Main headline export function
(defun org-iorg-headline (headline contents info)
  "Transcode element HEADLINE into HTML syntax.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (if (not (member "iorg" (org-export-get-tags headline info)))
      ;; Fallback to regular HTML.
      (funcall (cdr (assq 'headline org-e-html-translate-alist))
               headline contents info)
    ;; Otherwise, build <form> template.
    (let* ((action (org-element-property :html-form headline))
           (submit (org-element-property :html-button-value headline))
           (numberedp (org-export-numbered-headline-p headline info))
           (level (org-export-get-relative-level headline info))
           (text (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (section-number (and (org-export-numbered-headline-p headline info)
                                (mapconcat 'number-to-string
                                           (org-export-get-headline-number
                                            headline info) ".")))
           ;; Create the headline text.
           (full-text (org-iorg-format-headline--wrap headline info)))
      
      (cond
       ;; Case 1: This is a footnote section: ignore it.
       ((org-element-property :footnote-section-p headline) nil)
       ;; Case 2. This is a deep sub-tree: export it as a list item.
       ;;         Also export as items headlines for which no section
       ;;         format has been found.
       ((org-export-low-level-p headline info) ; FIXME (or (not section-fmt))
        ;; Build the real contents of the sub-tree.
        (let* ((type (if numberedp 'unordered 'unordered)) ; FIXME
               (itemized-body (org-e-html-format-list-item
                               contents type nil nil full-text)))
          (concat
           (and (org-export-first-sibling-p headline)
                (org-e-html-begin-plain-list type))
           itemized-body
           (and (org-export-last-sibling-p headline)
                (org-e-html-end-plain-list type)))))
       ;; Case 3. Standard headline.  Export it as a section.
       (t
        (let* ((section-number (mapconcat 'number-to-string
                                          (org-export-get-headline-number
                                           headline info) "-"))
               (ids (remove 'nil
                            (list (org-element-property :custom-id headline)
                                  (org-element-property :id headline)
                                  (concat "sec-" section-number))))
               (preferred-id (car ids))
               (extra-ids (cdr ids))
               (extra-class (org-element-property :html-container-class headline))
               (level1 (+ level (1- org-e-html-toplevel-hlevel))))
          (format "<div id=\"%s\" class=\"%s\">%s%s%s</div>\n"
                  (format "outline-container-%s"
                          (if (zerop (length extra-ids)) section-number
                            preferred-id))
                  (concat (format "outline-%d" level1) (and extra-class " ")
                          extra-class)
                  (format "<form method=post action=\"%s\">\n" action)
                  (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                          level1
                          preferred-id
                          (mapconcat
                           (lambda (x)
                             (let ((id (org-solidify-link-text
                                        (if (org-uuidgen-p x) (concat "ID-" x)
                                          x))))
                               (format "<a id=\"%s\" name=\"%s\"></a>" id id)))
                           extra-ids "")
                          full-text
                          level1)
                  (concat "<table border=0>\n"
                          contents
                          "<tr>\n"
                          (format "<td><input type=\"reset\"></td>\n")
                          (format "<td><input type=\"submit\" value=\"%s\"></td>\n" submit)
                          "</tr>\n"
                          "</table>\n"
                          "</form>"))))))))


;; Format headline
(defun* org-iorg-format-headline   
  (todo todo-type priority text tags
	&key level section-number headline-label &allow-other-keys)
  (let ((section-number
	 (when section-number
	   (format "<span class=\"section-number-%d\">%s</span> "
		   level section-number)))
	(todo (org-iorg--todo todo))
	(tags (org-iorg--tags tags)))
    (concat section-number todo (and todo " ") text
	    (and tags "&nbsp;&nbsp;&nbsp;") tags)))


;; Wrap headline
(defun org-iorg-format-headline--wrap
  (headline info &optional format-function &rest extra-keys)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (+ (org-export-get-relative-level headline info)
		   (1- org-e-html-toplevel-hlevel)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data (org-element-property :title headline) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (headline-label (or (org-element-property :custom-id headline)
			     (concat "sec-" (mapconcat 'number-to-string
						       headline-number "-"))))
	 (format-function (cond
			   ((functionp format-function) format-function)
			   ((functionp org-e-html-format-headline-function)
			    (function*
			     (lambda (todo todo-type priority text tags
					   &allow-other-keys)
			       (funcall org-e-html-format-headline-function
					todo todo-type priority text tags))))
			   (t 'org-iorg-format-headline))))
    (apply format-function
    	   todo todo-type  priority text tags
    	   :headline-label headline-label :level level
    	   :section-number section-number extra-keys)))

;;; Transcode Helpers
;; Wrap Todo in select-box
(defun org-iorg--todo (todo)
  (when todo
    (concat "<span class=\"selectbox\">"
            "<select name:\"simple-todo\" size=\"1\">"
            "<option selected>TODO</option>"
            "<option>DONE</option>"
            "<option>WAITING</option>"
            "<option>CANCELLED</option>"
            "<option>HOLD</option>"
            "<option>NEXT</option>"
            "</select>"
            "</span>")))


;; Wrap Tags in text-field
(defun org-iorg--tags (tags)
  (when tags
    (concat "<span class=\"textfield\">"
            "<input type=\"text\" name=\"simple-tag\" size=\"3\" "
            "maxlenght=\"6\" value=\"bar\">"
            "</input>"
            "</span>")))


;;;; Section

(defun org-iorg-section (section contents info)
  "Transcode element HEADLINE into HTML syntax.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (let ((headline (org-export-get-parent section)))
    (if (or (not headline)
            (not (member "iorg"
                         (org-export-get-tags headline info))))
        ;; Fallback to regular HTML.
        (funcall (cdr (assq 'section org-e-html-translate-alist))
                 section contents info)
      ;; Otherwise, export CONTENTS as-is.
      contents)))

(defun org-iorg-paragraph (paragraph contents info)
  "Transcode element PARAGRAPH into HTML syntax.
CONTENTS is the contents of the paragraph.  INFO is a plist used
as a communication channel."
  (let ((headline (org-export-get-parent-headline paragraph)))
    (if (or (not headline)
            (not (member "iorg" (org-export-get-tags headline info))))
        ;; Fallback to regular HTML.
        (funcall (cdr (assq 'paragraph org-e-html-translate-alist))
                 paragraph contents info)
      (let ((attributes (org-export-read-attribute :attr_html paragraph)))
        (cond
         ;; If the paragraph is contained within an item, do not make
         ;; it a textarea.
         ((catch 'item-found
            (mapc (lambda (parent)
                    (when (eq (org-element-type parent) 'item)
                      (throw 'item-found t)))
                  (org-export-get-genealogy paragraph))
            nil)
          contents)
         ;; If paragraph has no special attribute, consider it
         ;; is plain text.
         ((not attributes)
          (format "<tr>\n<td colspan=2>\n%s</td>\n</tr>" contents))
         ;; Otherwise build <textarea> template.
         (t (format "<tr>
<td>%s</td>
<td>
<textarea%s>\n%s</textarea>
</td>
</tr>"
                    (or (plist-get attributes :title) "")
                    (let (options)
                      (mapc
                       (lambda (prop)
                         (let ((value (plist-get attributes prop)))
                           (when value
                             (setq options
                                   (concat options
                                           (format " %s=\"%s\""
                                                   (substring (symbol-name prop)
                                                              1)
                                                   value))))))
                       '(:cols :rows :name :readonly :value))
                      options)
                    contents)))
        ))))

(defun org-iorg-plain-list (plain-list contents info)
  "Transcode element PLAIN-LIST into HTML syntax.
CONTENTS is the contents of the plain-list.  INFO is a plist used
as a communication channel."
  (let ((headline (org-export-get-parent-headline plain-list)))
    (if (or (not headline)
            (not (member "iorg" (org-export-get-tags headline info))))
        ;; Fallback to regular HTML.
        (funcall (cdr (assq 'plain-list org-e-html-translate-alist))
                 plain-list contents info)
      ;; If plain-list is descriptive make it a select menu, otherwise
      ;; simply return CONTENTS as-is.
      (let ((attributes (org-export-read-attribute :attr_html plain-list)))
        (if (eq (org-element-property :type plain-list) 'descriptive)
            (format "<tr>
<td>%s</td>
<td>
<select name=\"%s\">\n%s</select>
</td>
</tr>"
                    (or (plist-get attributes :name) "")
                    contents)
          contents)))))

(defun org-iorg-item (item contents info)
  "Transcode element ITEM into HTML syntax.
CONTENTS is the contents of the ITEM.  INFO is a plist used as
a communication channel."
  (let ((headline (org-export-get-parent-headline item)))
    (if (or (not headline)
            (not (member "iorg" (org-export-get-tags headline info))))
        ;; Fallback to regular HTML.
        (funcall
         (cdr (assq 'item org-e-html-translate-alist))
              item contents info)
      ;; Otherwise find appropriate input type and build tag.
      ;; Attributes are read from parent plain-list since items have
      ;; no affiliated keyword attached to them.
      (let ((plain-list (org-export-get-parent item)))
        ;; List is descriptive: item is an option line whose value is
        ;; item's tag.
        (if (eq (org-element-property :type plain-list) 'descriptive)
            (format "<option value=\"%s\">%s</option>"
                    (org-export-data (org-element-property :tag item) info)
                    (org-trim contents)))
        ;; Otherwise build appropriate input type.  Assume item's
        ;; contents is the text before the input tag.
        (let* ((attributes (org-export-read-attribute :attr_html plain-list))
               (checkboxp (org-element-property :checkbox item))
               (type (if checkboxp 'checkbox (plist-get attributes :type))))
          (format "<tr>
<td>%s</td>
<td><input type=%s name=\"%s\"%s></td>
</tr>"
                  contents
                  type
                  (or (plist-get attributes :name) "")
                  (cond ((not (eq type 'checkbox)) "")
                        ((eq checkboxp 'on) " checked")
                        (t " unchecked"))))))))


(defun org-iorg-export-to-html
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-e-html-extension))
	 (file (org-export-output-file-name extension subtreep pub-dir))
	 (org-export-coding-system org-e-html-coding-system))
    (org-export-to-file 'iorg file subtreep visible-only body-only ext-plist)))


