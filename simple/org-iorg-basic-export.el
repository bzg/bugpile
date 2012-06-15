;;; org-iorg-b-export -- basic back-end for exporting Org files to interactive HTML

(require 'org-export)
(require 'org-element)
(require 'org-e-html)
;; (require 'org-iorg)

;;;; Define derived backend

(org-export-define-derived-backend iorg e-html
  :translate-alist ((headline . org-iorg-b-headline)
                    (item . org-iorg-b-item)
                    (paragraph . org-iorg-b-paragraph)
                    (plain-list . org-iorg-b-plain-list)
                    (section . org-iorg-b-section)))


;;;; Customisation group

(defgroup org-iorg-b-export nil
  "Options for exporting Org files to dynamic html."
  :tag "Org iOrg Export"
  :group 'org-iorg)

;;;; Headline
;; Headline customization variables
(defcustom org-iorg-b-format-headline-function nil
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
  :group 'org-iorg-b-export
  :type 'function)


;; Main headline export function
(defun org-iorg-b-headline (headline contents info)
  "Transcode element HEADLINE into HTML syntax.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (if (not (member "iorg" (org-export-get-tags headline info)))
      ;; Fallback to regular HTML.
      (funcall (cdr (assq 'headline org-e-html-translate-alist))
               headline contents info)
    ;; Otherwise, build <form> template.
    (let* (;; <form> related Vars
           (action (org-element-property :html-form headline))
           (submit (org-element-property :html-button-value headline))
           ;; headline attributes
           (numberedp (org-export-numbered-headline-p headline info))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (section-number (and (org-export-numbered-headline-p headline info)
                                (mapconcat 'number-to-string
                                           (org-export-get-headline-number
                                            headline info) ".")))
           (section-number-dash (mapconcat 'number-to-string
                                           (org-export-get-headline-number
                                            headline info) "-"))
           (text (org-export-data (org-element-property :title headline) info))
           ;; TODO's
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           ;; TAG's
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           ;; ID's
           (ids (remove 'nil
                        (list (org-element-property :custom-id headline)
                              (org-element-property :id headline)
                              (concat "sec-" section-number-dash))))
           (preferred-id (car ids))
           (extra-ids (cdr ids))
           (headline-number (org-export-get-headline-number headline info))
           (headline-label (or (org-element-property :custom-id headline)
                               (concat "sec-" (mapconcat 'number-to-string
                                                         headline-number "-"))))

           ;; class
           (extra-class (org-element-property :html-container-class headline))
           ;; Level
           (level-wrap (+ (org-export-get-relative-level headline info)
                          (1- org-e-html-toplevel-hlevel)))
           (level (org-export-get-relative-level headline info))
           (level1 (+ level (1- org-e-html-toplevel-hlevel)))
           ;; Create the headline text.
           (full-text (org-iorg-b-format-headline--wrap headline info)))

      (format "<div id=\"%s\" class=\"%s\">%s%s%s</div>\n"
              ;; id
              (format "outline-container-%s"
                      (if (zerop (length extra-ids))
                          section-number-dash
                        preferred-id))
              ;; class
              (concat (format "outline-%d" level1) (and extra-class " ")
                      extra-class)
              ;; <form>
              (format "<form method=post action=\"%s\">\n" action)
              ;; formatted headline
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
                      (org-iorg-b-format-headline
                       todo todo-type  priority text tags
                       :headline-label headline-label :level level-wrap
                       :section-number section-number
                       ;extra-keys
                       )
                     ;full-text  REPLACE!
                      level1)
              ;; content
              (concat contents
                      ;; <form> buttons
                      "<table border=0>\n"
                      "<tr>\n"
                      (format "<td><input type=\"reset\"></td>\n")
                      (format "<td><input type=\"submit\" value=\"%s\"></td>\n" submit)
                      "</tr>\n"
                      "</table>\n"
                      "</form>")))))


;; Format headline
(defun* org-iorg-b-format-headline   
  (todo todo-type priority text tags
	&key level section-number headline-label &allow-other-keys)
  (let ((section-number
	 (when section-number
	   (format "<span class=\"section-number-%d\">%s</span> "
		   level section-number)))
	(todo (org-iorg-b--todo todo))
	(tags (org-iorg-b--tags tags)))
    (concat section-number todo (and todo " ") text
	    (and tags "&nbsp;&nbsp;&nbsp;") tags)))
	 ;; 
	 ;; (section-number (and (org-export-numbered-headline-p headline info)
	 ;;        	      (mapconcat 'number-to-string
	 ;;        			 headline-number ".")))
	 ;; (todo (and (plist-get info :with-todo-keywords)
	 ;;            (let ((todo (org-element-property :todo-keyword headline)))
	 ;;              (and todo (org-export-data todo info)))))
	 ;; (todo-type (and todo (org-element-property :todo-type headline)))

           ;; (priority (and (plist-get info :with-priority)
           ;;                (org-element-property :priority headline)))
	 ;; (text (org-export-data (org-element-property :title headline) info))
	 ;; (tags (and (plist-get info :with-tags)
	 ;;            (org-export-get-tags headline info)))



;;; Transcode Helpers
;; Wrap Todo in select-box
(defun org-iorg-b--todo (todo)
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
(defun org-iorg-b--tags (tags)
  (when tags
    (concat "<span class=\"textfield\">"
            "<input type=\"text\" name=\"simple-tag\" size=\"3\" "
            "maxlenght=\"6\" value=\"bar\">"
            "</input>"
            "</span>")))


;;;; Section

(defun org-iorg-b-section (section contents info)
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

(defun org-iorg-b-paragraph (paragraph contents info)
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

(defun org-iorg-b-plain-list (plain-list contents info)
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

(defun org-iorg-b-item (item contents info)
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


(defun org-iorg-b-export-to-html
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


