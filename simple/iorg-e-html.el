;; iorg-e-html -- interactve variants of html transcoding functions



;;;; Headline

(defcustom iorg-e-html-format-headline-function nil
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
  :group 'org-export-e-html
  :type 'function)



;;; Transcode Helpers

;;;; Todo

(defun iorg-e-html--todo (todo)
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


;;;; Tags

(defun iorg-e-html--tags (tags)
  (when tags
    (concat "<span class=\"textfield\">"
            "<input type=\"text\" name:\"simple-tag\""
                    "size=\"3\" maxlenght=\"6\" value=\"bar\">"
            "</input>"
            "</span>")))

;;;; Headline

(defun* iorg-e-html-format-headline   
  (todo todo-type priority text tags
	&key level section-number headline-label &allow-other-keys)
  (let ((section-number
	 (when section-number
	   (format "<span class=\"section-number-%d\">%s</span> "
		   level section-number)))
	(todo (iorg-e-html--todo todo))
	(tags (iorg-e-html--tags tags)))
    (concat section-number todo (and todo " ") text
	    (and tags "&nbsp;&nbsp;&nbsp;") tags)))


(defun iorg-e-html-format-headline--wrap (headline info
						  &optional format-function
						  &rest extra-keys)
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
		    (org-element-property :tags headline)))
	 (headline-label (concat "sec-" (mapconcat 'number-to-string
						   headline-number "-")))
	 (format-function (cond
			   ((functionp format-function) format-function)
			   ((functionp iorg-e-html-format-headline-function)
			    (function*
			     (lambda (todo todo-type priority text tags
					   &allow-other-keys)
			       (funcall iorg-e-html-format-headline-function
					todo todo-type priority text tags))))
			   (t 'iorg-e-html-format-headline))))
    (apply format-function
    	   todo todo-type  priority text tags
    	   :headline-label headline-label :level level
    	   :section-number section-number extra-keys)))


;; FIXME: where is it called??
(defun iorg-e-html-headline  (headline contents info)
  "Transcode an HEADLINE element from Org to HTML, conditional on iOrg information.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-data (org-element-property :title headline) info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-element-property :tags headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 (org-export-get-headline-number
					  headline info) ".")))
	 ;; Create the headline text.
	 (full-text (iorg-e-html-format-headline--wrap headline info)))
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
	 (and (org-export-first-sibling-p headline info)
	      (org-e-html-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-e-html-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((extra-class (org-element-property :html-container-class headline))
	     (extra-ids (list (org-element-property :custom-id headline)
			      (org-element-property :id headline)))
	     (extra-ids
	      (mapconcat
	       (lambda (x)
		 (when x
		   (let ((id (org-solidify-link-text
			      (if (org-uuidgen-p x) (concat "ID-" x) x))))
		     (format "<a id=\"%s\" name=\"%s\"/>" id id))))
	       extra-ids ""))
	     (level1 (+ level (1- org-e-html-toplevel-hlevel)))
	     (id (mapconcat 'number-to-string
			    (org-export-get-headline-number headline info) "-")))
	(format "<div id=\"%s\" class=\"%s\">%s%s</div>\n"
		(format "outline-container-%s" id)
		(concat (format "outline-%d" level1) (and extra-class " ")
			extra-class)
		(format "\n<h%d id=\"sec-%s\">%s%s</h%d>\n"
			level1 id extra-ids full-text level1)
		contents))))))




;;;; Section
;; FIXME: where is it called??
(defun iorg-e-html-section (section contents info) ; FIXME
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section info))
        (raw-org-contents (plist-get info :)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let ((class-num (+ (org-export-get-relative-level parent info)
			  (1- org-e-html-toplevel-hlevel)))
            (id-num
             (mapconcat
              'number-to-string
	      (org-export-get-headline-number parent info) "-")))
        ;; Build return value.
        (format "<div class=\"outline-text-%d\" id=\"text-%s\"><textarea name=\"simple-content\" cols=\"79\" rows=\"30\">\n%s</textarea>/</div>"
                class-num id-num contents)))))

