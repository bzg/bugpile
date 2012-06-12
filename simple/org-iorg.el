;;; org-iorg -- back-end for exporting Org files to interactive HTML

(org-export-define-derived-backend iorg e-html
  :translate-alist ((headline . org-iorg-headline)
                    (item . org-iorg-item)
                    (paragraph . org-iorg-paragraph)
                    (plain-list . org-iorg-plain-list)
                    (section . org-iorg-section)))

(defun org-iorg-headline (headline contents info)
  "Transcode element HEADLINE into HTML syntax.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (if (not (member "iorg" (org-export-get-tags headline info)))
      ;; Fallback to regular HTML.
      (funcall (cdr (assq 'headline org-e-html-translate-alist)
               headline contents info))
    ;; Otherwise, build <form> template.
    (let ((action (org-element-property :html-form headline))
          (submit (org-element-property :html-button-value headline)))
      (concat (format "<form method=post action=\"%s\">\n" action)
              "<table border=0>\n"
              contents
              "<tr>\n"
              (format "<td><input type=\"reset\"></td>\n")
              (format "<td><input type=\"submit\" value=\"%s\"></td>\n" submit)
              "</tr>\n"
              "</table>\n"
              "</form>"))))

(defun org-iorg-section (section contents info)
  "Transcode element HEADLINE into HTML syntax.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (let ((headline (org-export-get-parent section)))
    (if (or (not headline)
            (not (member "iorg"
                         (org-export-get-tags headline info))))
        ;; Fallback to regular HTML.
        (funcall (cdr (assq 'section org-e-html-translate-alist)
                 section contents info))
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
        (funcall (cdr (assq 'paragraph org-e-html-translate-alist)
                 paragraph contents info))
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
        (funcall (cdr (assq 'plain-list org-e-html-translate-alist)
                 plain-list contents info))
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
         (cdr (assq 'item org-e-html-translate-alist)
              item contents info)) 
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


