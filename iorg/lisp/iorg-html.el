;;;; iorg-html -- basic back-end for exporting Org files to interactive HTML

;;;; Requirements

;;; Require Packages
(require 'org-element)
;; (require 'org-export)
;; (require 'org-e-html)
;; (require 'iorg-util)

;;; Other Stuff

;; Define iorg-html as derived backend for org-export. Enables e.g.
;; (org-export-to-file 'iorg-html ...)
(org-export-define-derived-backend iorg-html e-html
  :translate-alist ((headline . iorg-html-headline)
                    (section . iorg-html-section)
                    (property-drawer . nil)
                    ;;(item . iorg-html-item)
                    ;; (paragraph . iorg-html-paragraph)
                    ;; (plain-list . iorg-html-plain-list)
                    ;; (property-drawer . iorg-html-property-drawer)
                    ))


;;;; Variables
;;; Consts

;;; Vars
(defvar iorg-html-property-prefix-regexp "^[^-]+"
  "Regexp that matches any prefix of an Org headline property key.")

;;; Customs

(defgroup iorg-html nil
  "Options for exporting Org files to dynamic html."
  :tag "Org iOrg Export"
  :group 'org-iorg)

(defcustom iorg-html-property-key-prefix-plist
  '(:export ("html" "bugpile") :noexport  nil) 
  "Alist with a list of prefix strings in the cdr that are used to identify those headline properties that will be made editable by the iOrg exporter."
  :group 'iorg-html
  :type 'plist)
 
;;;; Functions
;;; Function Declarations


;;; Helper Functions (general)
(defun iorg-html--read-from-input-file (file beg end)
  "Return buffer substring between characters BEG and END from Org input file FILE, given as absolute file-name."
  (if (not (and
            (file-exists-p file)
            (number-or-marker-p beg)
            (number-or-marker-p end)))
      (error: "File doesn't exists or START and END arguments are not numbers or markers")
    (with-current-buffer (find-file file)
      (save-excursion
        (save-restriction
          (widen)
          (buffer-substring-no-properties beg end))))))


(defun iorg-html--get-org-input (element info &optional property-drawer)
  "Return content of input Org file"
  (let* ((input-file (plist-get info :input-file))
         (beg (or (and property-drawer
                       (org-element-property :end property-drawer))
                  (org-element-property :begin element)))
         (end (org-element-property :end element)))
    (iorg-html--read-from-input-file input-file beg end)))


(defun iorg-html--generate-property-key-regexp (prefix)
  "Generate a regexp that matches all Org headline properties whose keys begin with PREFIX followed by a dash '-'."
  (if (not (non-empty-string-p prefix))
      (error "PREFIX must be a non-empty string")
    (format "%s-[-[:alpha:]]+" prefix)))
  

;;; Helper Functions (transcode headline)
(defun iorg-html--headline-todo (headline info)
  "Wrap headline todo's in html select-box, reading values from the HEADLINE and INFO arguments"
  (let* ((todo (and (plist-get info :with-todo-keywords)
                    (let ((todo (org-element-property :todo-keyword headline)))
                      (and todo (org-export-data todo info)))))
         ;; (todo-type (and todo (org-element-property :todo-type headline)))
         )

    (when (and
           todo
           org-todo-keywords-for-agenda
           (member todo org-todo-keywords-for-agenda))
      (format "%s%s%s"
              (concat "<span class=\"selectbox\">"
                      "<select name:\"simple-todo\" size=\"1\">"
                      ;; FIXME generate unique value
                      "<option value=\"1\" selected>")
              todo
              (concat "</option>"
                      (mapconcat
                       (lambda (x)
                         (format
                          "<option>%s</option>" x))
                       (remove todo org-todo-keywords-for-agenda) "")
                      "</select>"
                      "</span>")))))

(defun iorg-html--headline-tags (headline info)
  "Wrap headline tags in html text-field, reading values from the HEADLINE and INFO arguments"
  (let* ((tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info))))

    (when tags
      ;; FIXME handle Orgs default tags (or ban them from export)
      (format "%s%s%s"
              (concat "<span class=\"textfield\">"
                      "<input type=\"text\" name=\"simple-tag\" size=\"10\""
                      "maxlenght=\"20\" value=\"")
              (mapconcat
               (lambda (x)
                 (format ":%s" x)) (remove "iorg" tags) "")
              (concat ":\">"
                      "</input>"
                      "</span>")))))

(defun iorg-html--headline-text (text)
  "Wrap headline TEXT in html text-field."
  (when text
    (format "%s%s%s"
            (concat "<span class=\"textfield\">"
                    "<input type=\"text\" name=\"simple-text\" size=\"40\" "
                    "maxlenght=\"80\" value=\"")
            text
            (concat "\">"
                    "</input>"
                    "</span>"))))



;;; Public Functions (interactive)

;; Exporting function
(defun iorg-html-export-to-html
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


;;; Public Functions (non-interactive)              

;; Main headline export function
(defun iorg-html-headline (headline contents info)
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
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (numberedp (org-export-numbered-headline-p headline info))
           (section-number (org-export-get-headline-number headline info))
           (section-number-point (and
                                  numberedp
                                  (mapconcat 'number-to-string section-number ".")))
           (section-number-dash (and
                                 numberedp
                                 (mapconcat 'number-to-string section-number "-")))
           ;; text
           (text (org-export-data (org-element-property :title headline) info))
           (text-formatted (iorg-html--headline-text text))
           ;; TODO's
           (todo-formatted (iorg-html--headline-todo headline info))
           ;; TAG's
           (tags-formatted (iorg-html--headline-tags headline info))
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
           ;; level
           (level-wrap (+ (org-export-get-relative-level headline info)
                          (1- org-e-html-toplevel-hlevel)))
           (level (org-export-get-relative-level headline info))
           (level1 (+ level (1- org-e-html-toplevel-hlevel)))
           ;; formatted section number
           (section-number-formatted
            (when section-number
              (format "<span class=\"section-number-%d\">%s</span> "
                      level section-number))))           

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
                      (concat
                       section-number-point
                       (and section-number-point "&nbsp;&nbsp;") ; FIXME where is the point?
                       todo-formatted
                       (and todo-formatted "&nbsp;")
                       text
                       (and tags-formatted "&nbsp;&nbsp;&nbsp;")
                       tags-formatted)
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

;; Section
(defun iorg-html-section (section contents info &optional key-prefix-list)
  "Transcode element HEADLINE into HTML syntax.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel.KEY-PREFIX-LIST
is an optional list of prefix strings.
"
  (let* ((headline (org-export-get-parent section))
         ;; Get the property drawer
         (prop-drawer
          (org-element-map
           section 'property-drawer
           (lambda (draw)
             draw) nil 'first-match))
         ;; Get the properties
         (properties (org-element-property :properties prop-drawer))
         ;; Get the prefixes that flag a property for export
         (prefix-list
          (or key-prefix-list
              (plist-get iorg-html-property-key-prefix-plist :export)))
         ;; Generate a set of all property-key prefixes in the drawer
         (prop-drawer-prefix-list
          (and
           prop-drawer
           (delete-dups
            (mapcar
             (lambda (x)
               (progn
                 (string-match
                  iorg-html-property-prefix-regexp (car x))
                 (match-string 0 (car x))))
             properties))))
         ;; Are there Org properties that need to be exported?
         (export-prop-p
          (and
           prop-drawer-prefix-list
           (mapc
            (lambda (x)
              (member x prefix-list))
            prop-drawer-prefix-list))))

    (if (or (not headline)
            (not (member "iorg"
                         (org-export-get-tags headline info))))
        ;; Fallback to regular HTML.
        (funcall (cdr (assq 'section org-e-html-translate-alist))
                 section contents info)
      ;; Otherwise
      (format "%s%s"
              (or
               ;; Are there properties that need to be exported?
               (and
                export-prop-p
                ;; Put the textfields in a table
                (format
                 "<table border=\"0\">%s</table>"
                 (mapconcat
                  (lambda (x)
                    ;;  Select the properties that need to be exported 
                    (if (member
                         (progn
                           (string-match
                            iorg-html-property-prefix-regexp
                            (car x))
                           (match-string 0 (car x)))
                         prefix-list)
                        ;; Generate one table row for each exported property
                        (format 
                         (concat
                          "<tr>"
                          "<td>%s</td>"
                          ;; FIXME generate unique names
                          "<td><input type=\"text\" name=\"simple-prop\""
                          "size=\"40\" maxlength=\"80\" value=\"%s\">"
                          "</input></td>"
                          "</tr>")
                         (car x)
                         (cdr x))))
                  properties "")))
               ;; Export (maybe only) the section content                  
               "")
              (format
               (concat
                "<textarea class=\"textarea\" name=\"simple-section\""
                "cols=\"80\" rows=\"35\">%s</textarea>")
               (iorg-html--get-org-input
                section info prop-drawer))))))


;;; Public Functions (obsolete?)              

;; ;; Property drawer

;; (defun iorg-html-property-drawer
;;   (property-drawer contents info)
;;   "Transcode element PROPERTY-DRAWER into HTML syntax. CONTENTS is the contents of the paragraph. INFO is a plist used as a communication channel."
;;     (format "%s" ""))
 
;; ;; Paragraph

;; (defun iorg-html-paragraph (paragraph contents info)
;;   "Transcode element PARAGRAPH into HTML syntax.
;; CONTENTS is the contents of the paragraph.  INFO is a plist used
;; as a communication channel."
;;   (let ((headline (org-export-get-parent-headline paragraph)))
;;     (if (or (not headline)
;;             (not (member "iorg" (org-export-get-tags headline info))))
;;         ;; Fallback to regular HTML.
;;         (funcall (cdr (assq 'paragraph org-e-html-translate-alist))
;;                  paragraph contents info)
;;       (let ((attributes (org-export-read-attribute :attr_html paragraph)))
;;         (cond
;;          ;; If the paragraph is contained within an item, do not make
;;          ;; it a textarea.
;;          ((catch 'item-found
;;             (mapc (lambda (parent)
;;                     (when (eq (org-element-type parent) 'item)
;;                       (throw 'item-found t)))
;;                   (org-export-get-genealogy paragraph))
;;             nil)
;;           contents)
;;          ;; If paragraph has no special attribute, consider it
;;          ;; is plain text.
;;          ((not attributes)
;;           (format "<tr>\n<td colspan=2>\n%s</td>\n</tr>" contents))
;;          ;; Otherwise build <textarea> template.
;;          (t (format "<tr>
;; <td>%s</td>
;; <td>
;; <textarea%s>\n%s</textarea>
;; </td>
;; </tr>"
;;                     (or (plist-get attributes :title) "")
;;                     (let (options)
;;                       (mapc
;;                        (lambda (prop)
;;                          (let ((value (plist-get attributes prop)))
;;                            (when value
;;                              (setq options
;;                                    (concat options
;;                                            (format " %s=\"%s\""
;;                                                    (substring (symbol-name prop)
;;                                                               1)
;;                                                    value))))))
;;                        '(:cols :rows :name :readonly :value))
;;                       options)
;;                     contents)))
;;         ))))

 
;; ;; Plain List

;; (defun iorg-html-plain-list (plain-list contents info)
;;   "Transcode element PLAIN-LIST into HTML syntax.
;; CONTENTS is the contents of the plain-list.  INFO is a plist used
;; as a communication channel."
;;   (let ((headline (org-export-get-parent-headline plain-list)))
;;     (if (or (not headline)
;;             (not (member "iorg" (org-export-get-tags headline info))))
;;         ;; Fallback to regular HTML.
;;         (funcall (cdr (assq 'plain-list org-e-html-translate-alist))
;;                  plain-list contents info)
;;       ;; If plain-list is descriptive make it a select menu, otherwise
;;       ;; simply return CONTENTS as-is.
;;       (let ((attributes (org-export-read-attribute :attr_html plain-list)))
;;         (if (eq (org-element-property :type plain-list) 'descriptive)
;;             (format "<tr>
;; <td>%s</td>
;; <td>
;; <select name=\"%s\">\n%s</select>
;; </td>
;; </tr>"
;;                     (or (plist-get attributes :name) "")
;;                     contents)
;;           contents)))))

;; (defun iorg-html-item (item contents info)
;;   "Transcode element ITEM into HTML syntax.
;; CONTENTS is the contents of the ITEM.  INFO is a plist used as
;; a communication channel."
;;   (let ((headline (org-export-get-parent-headline item)))
;;     (if (or (not headline)
;;             (not (member "iorg" (org-export-get-tags headline info))))
;;         ;; Fallback to regular HTML.
;;         (funcall
;;          (cdr (assq 'item org-e-html-translate-alist))
;;               item contents info)
;;       ;; Otherwise find appropriate input type and build tag.
;;       ;; Attributes are read from parent plain-list since items have
;;       ;; no affiliated keyword attached to them.
;;       (let ((plain-list (org-export-get-parent item)))
;;         ;; List is descriptive: item is an option line whose value is
;;         ;; item's tag.
;;         (if (eq (org-element-property :type plain-list) 'descriptive)
;;             (format "<option value=\"%s\">%s</option>"
;;                     (org-export-data (org-element-property :tag item) info)
;;                     (org-trim contents)))
;;         ;; Otherwise build appropriate input type.  Assume item's
;;         ;; contents is the text before the input tag.
;;         (let* ((attributes (org-export-read-attribute :attr_html plain-list))
;;                (checkboxp (org-element-property :checkbox item))
;;                (type (if checkboxp 'checkbox (plist-get attributes :type))))
;;           (format "<tr>
;; <td>%s</td>
;; <td><input type=%s name=\"%s\"%s></td>
;; </tr>"
;;                   contents
;;                   type
;;                   (or (plist-get attributes :name) "")
;;                   (cond ((not (eq type 'checkbox)) "")
;;                         ((eq checkboxp 'on) " checked")
;;                         (t " unchecked"))))))))


(provide 'iorg-html)
