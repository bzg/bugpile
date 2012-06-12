;; iorg-e-html -- interactve variants of html transcoding functions

;;;; Format Headline Function

;; (defcustom org-e-html-format-headline-function nil
;;   "Function to format headline text."...)


(setq
 org-e-html-format-headline-function
 'iorg-e-html-format-headline)

;;; Transcode Helpers
;;;; Wrap Todo in select-box
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


;;;; Wrap Tags in text-field
(defun iorg-e-html--tags (tags)
  (when tags
    (concat "<span class=\"textfield\">"
            "<input type=\"text\" name:\"simple-tag\""
                    "size=\"10\" maxlenght=\"30\" value=\"foo\">"
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

