;;; simple.el -- iorg proof of concept
(require 'org)
(require 'elnode)
(require 'org-export)
(require 'org-e-html)
(require 'org-agenda)
(require 'util)

;;;; Declare functions
(declare-function org-entry-is-todo-p "org" nil)
(declare-function org-get-todo-state "org" nil)
(declare-function org-check-for-org-mode "org-agenda" nil)

;; FIXME delete this line:
(setq org-export-filter-headline-function nil)

;; (add-to-list 'org-export-filter-final-output-functions
;;              'iorg-html-postprocess)

(add-to-list 'org-export-filter-headline-functions
             'iorg-html-wrap-headline-content-in-textarea)

;; (add-to-list 'org-export-filter-headline-functions
;;              'iorg-html-wrap-headline-in-form)

;; (add-to-list 'org-export-filter-headline-functions
;;              'iorg-html-wrap-headline-todo-in-select)

;; (add-to-list 'org-export-filter-headline-functions
;;              'iorg-html-wrap-headline-string-in-text)

;; (add-to-list 'org-export-filter-headline-functions
;;              'iorg-html-wrap-headline-tags-in-text)

 
(defvar iorg-html-outline-text-regexp
  (concat
   ;; div
   "\\(<div\\)"
   ;; blank(s)
   "\\([[:blank:]]+\\)"
   ;; class
   "\\(class=\"outline-text-\\)\\([-[:digit:]]+\\)\\(\"\\)"
   ;; blank(s)
   "\\([[:blank:]]+\\)"
   ;; id
   "\\(id=\"text\\)\\([-[:digit:]]+\\)\\(\">\\)")
  "Match the beginning of outline text in exported html.")

(defvar iorg-html-outline-container-regexp
  (concat
   ;; div
   "\\(<div\\)"
   ;; blank(s)
   "\\([[:blank:]]+\\)"
   ;; id
   "\\(id=\"outline-container\\)\\([-[:digit:]]+\\)\\(\"\\)"
   ;; blank(s)
   "\\([[:blank:]]+\\)"
   ;; class
   "\\(class=\"outline\\)\\([-[:digit:]]+\\)\\(\">\\)")
  "Match the beginning of an outline-container in exported html.")

(defvar iorg-html-todo-regexp
  (concat
   ;; span
  "\\(<span\\)"
  ;; blank(s)
   "\\([[:blank:]]+\\)"
  ;; class
  "\\(class=\"todo\\|class=\"done\\)"
  ;; blank(s)
   "\\([[:blank:]]+\\)"
  ;; TODO
  "\\([A-Z]+\\)\\(\">\\)")
  "Match todo items in exported html.")

(defvar iorg-html-tag-regexp
  (concat
   ;; span
  "\\(<span\\)"
  ;; blank(s)
   "\\([[:blank:]]+\\)"
  ;; class
  "\\(class=\"tag\">\\)")
  "Match todo items in exported html.")

(defvar iorg-html-section-number-regexp
  (concat
   ;; span
  "\\(<span\\)"
  ;; blank(s)
   "\\([[:blank:]]+\\)"
  ;; class
  "\\(class=\"section-number\\)\\([-[:digit:]]+\\)\\(\">\\)")
  "Match section-number in exported html.")

(defvar iorg-html-div-end-repexp
  "</div>"
  "Match end of <div> sections")

(defvar iorg-html-span-end-repexp
  "</span>"
  "Match end of <span> sections")

;; (defvar iorg-input-submit-regexp
;;   "<input type=\"submit\""
;;   "Match submit button in exported html.")

(defvar iorg-alist-outline-regexp
  "\\(outline\\)\\([-[:digit:]]+\\)"
  "Match key in http-params alist that identifies the outline level.")

(defconst simple-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The project directory of the 'simple' app in canonical form")

(defconst simple-urls
  '(("^$"      . iorg-initialize-simple-handler)
    ("^todo/$" . iorg-change-state-handler)
    ("^edit/$" . iorg-edit-headline-handler)))

(defun simple-dispatcher-handler (httpcon)
  "Dispatch requests to the 'simple' app"
  (elnode-log-access "simple" httpcon)
  (elnode-dispatcher httpcon simple-urls))

(defun iorg-html-postprocess (transc-str back-end comm-chan)
  "Add buttons to HTML export to make headlines editable."
  ;; TODO: (2) adding buttons to html export
  (with-temp-buffer
    (insert transc-str)
    (goto-char (point-min))
    (while (and
            (re-search-forward iorg-html-todo-regexp nil t)
            (re-search-forward iorg-html-outline-text-regexp nil t))
      (goto-char (match-beginning 0))
      (insert
       (concat
        "<form action=\"http://localhost:8031/todo/\">"
        "  <input type=\"submit\" value=\" Finish \" name=\"outline-1\">"
        "</form>")))
    (buffer-substring-no-properties (point-min) (point-max))))


(defun iorg-html-wrap-headline-todo-in-select (transc-str back-end comm-chan) 
  "Wrap the todo part of the (BACK-END = html) transcoded Org headline TRANSC-STR into a html select-field, using information from the communication channel COMM-CHAN."
    (with-temp-buffer
    (insert transc-str)
    (goto-char (point-min))
    (while (re-search-forward iorg-html-todo-regexp nil t)
      (goto-char (match-end 0))
      (insert
       (concat
        "<select "
        "name=\"simple-todo\""
        "size=\"1\">"
        "<option selected>TODO</option>"
        "<option>DONE</option>")))
    (while (re-search-forward iorg-html-span-end-repexp nil t)
      (goto-char (match-beginning 0))
      (insert "<\select>"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun iorg-html-wrap-headline-tags-in-text (transc-str back-end comm-chan) 
  "Wrap the tags in the (BACK-END = html) transcoded Org headline TRANSC-STR into one html text-field each, using information from the communication channel COMM-CHAN."
    (with-temp-buffer
    (insert transc-str)
    (goto-char (point-min))
    (while (re-search-forward iorg-html-tag-regexp nil t)
      (goto-char (match-end 0))
      (insert
       (concat
        "<input "
        "type=\"text\""
        "name=\"simple-tag\""
        "size=\"40\""
        "maxlenght=\"80\">")))
    (while (re-search-forward iorg-html-span-end-repexp nil t)
      (goto-char (match-end 0))
      (insert "<\input>"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun iorg-html-wrap-headline-string-in-text (transc-str back-end comm-chan) 
  "Wrap the headline string in the (BACK-END = html) transcoded Org headline TRANSC-STR into a html text-field, using information from the communication channel COMM-CHAN."
  (with-temp-buffer
    (insert transc-str)
    (goto-char (point-min))
    (and
     (re-search-forward "&nbsp;&nbsp;&nbsp;" nil t)
     (re-search-backward iorg-html-span-end-repexp nil t)
     (progn
       (goto-char (match-end 0))
       (insert
        (concat
         "<input "
         "type=\"text\""
         "name=\"simple-headline\""
         "size=\"40\""
         "maxlenght=\"80\">"))
       (and (re-search-forward "<" nil t)
            (goto-char (match-beginning 0))
            (insert "<\input>"))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun iorg-html-wrap-headline-in-form (transc-str back-end comm-chan) 
  "Wrap the headline in the (BACK-END = html) transcoded Org headline TRANSC-STR into a html form, using information from the communication channel COMM-CHAN."
    (with-temp-buffer
    (insert transc-str)
    (goto-char (point-min))
    (while (re-search-forward iorg-html-outline-container-regexp nil t)
      (goto-char (match-end 0))
      (insert "<form action=\"http://localhost:8031/edit/\">"))
    (while (re-search-forward iorg-html-div-end-repexp nil t)
      (goto-char (match-beginning 0))
      (insert
        "<input type=\"submit\" value=\" Edit \" name=\"outline-container-1\">"
        "</form>"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun iorg-html-wrap-headline-content-in-textarea (transc-str back-end comm-chan) 
  "Wrap the content of the (BACK-END = html) transcoded Org headline TRANSC-STR into a html textarea, using information from the communication channel COMM-CHAN."
    ;; TODO delete this line:
    (message "Raw value headline: %S" (org-element-property :raw-value headline))
    (message "Content headline: %s" (org-element-contents headline))
    (with-temp-buffer
    (insert transc-str)
    (goto-char (point-min))
    (while (re-search-forward iorg-html-outline-text-regexp nil t)
      (goto-char (match-end 0))
      (insert
       (concat
        "<textarea name=\"simple-content\""
        "cols=\"79\" rows=\"30\">" )))
    (and (re-search-forward iorg-html-div-end-repexp nil t)
      (goto-char (match-beginning 0))
      (insert "<\textarea>"))
    (buffer-substring-no-properties (point-min) (point-max))))


(defun iorg-launch (port)
  "Launch the elnode server which will serve and edit simple.org."
  ;; TODO: (1) elnode serving simple.org to html
  (interactive "nPort number: ")
  (elnode-start
   'simple-dispatcher-handler
   :port port :host "localhost"))

 (defun iorg-initialize-simple-handler (httpcon)
  "Serves the start-page of the 'simple' app"
  (elnode-send-file httpcon (iorg--org-to-html "simple.org")))

 (defun iorg-change-state-handler (httpcon)
  "Called by the elnode form handler to update task state."
  ;; TODO: (3) handle form post data and update an Org-mode file
  (message "entering `iorg-change-state-handler'")  
  (let ((params (elnode-http-params httpcon)))
    (message "These are the http-params: \n %s" params)
    (with-current-buffer
        (find-file (expand-file-name "simple.org" simple-dir))
      (save-excursion
        (iorg--params-find-entry params)
        (org-todo 'done))
      (save-buffer)
      ;(kill-buffer (current-buffer))
      )
    (iorg-initialize-simple-handler httpcon)))


;; (defun iorg-edit-headline-handler (httpcon)
;;   "Called by the elnode form handler to update headline."
;;   ;; TODO: (3) handle form post data and update an Org-mode file
;;   (message "entering `iorg-edit-headline-handler'")  
;;   (let ((params (elnode-http-params httpcon)))
;;     (message "These are the http-params: \n %s" params)
;;     (with-current-buffer
;;         (find-file (expand-file-name "simple.org" simple-dir))
;;       (save-excursion
;;         ;; (iorg--params-find-entry params)
;;         ;; (org-todo 'done))
;;       (save-buffer)
;;       ;(kill-buffer (current-buffer))
;;       )
;;     (iorg-initialize-simple-handler httpcon))))
  

(defun iorg--get-outline-level (param-list)
  "Return level of outline-tree encoded in http-params"
  (and
   (listp param-list)
   (assoc-re iorg-alist-outline-regexp param-list 2)))


(defun iorg--normalize-outline-level (outline-level)
  "Normalize OUTLINE-LEVEL in the format \"[-[:digit:]]+\" to a
list of numbers (as strings). The lenght of the returned list is
equal to the number of sublevels we need to walk down in the
outline tree, the value of each number identifies the nth-entry
in the Org file on that level."
(if (not
     (and
      (iorg--stringp outline-level)
      (string-match "[-[:digit:]]+" outline-level)))
    (error "Wrong type or format of OUTLINE-LEVEL argument")
  (delete "" (split-string outline-level "-"))))

 (defun iorg--params-find-entry (param-list &optional file)
  "Go to the entry in the current Org buffer that is specified in the PARAM-LIST"
  (condition-case err
      (let* ((outline-level
              (iorg--get-outline-level param-list))
             (normalized-outline-level
              (iorg--normalize-outline-level outline-level))
             (sublevel-p nil))
        (with-current-buffer
            (if (and file (file-exists-p file))
                (find-file file)
             (find-file (expand-file-name "simple.org" simple-dir)))
          (org-check-for-org-mode)
          (save-restriction
            (widen)
            (iorg--goto-first-entry)
            (mapc
             (lambda (n)
               (if sublevel-p
                   (outline-next-heading))
               (org-forward-same-level
                (1- (string-to-number n)) "INVISIBLE-OK")
               (unless sublevel-p
                 (setq sublevel-p 1)))
             normalized-outline-level))))
    (error "Error while going to outline entry specified in PARAM-LIST: %s " err)))

(defun iorg--org-to-html (org-file)
  "Export ORG-FILE to html and return the expanded filename"
  (if (not (file-exists-p (expand-file-name org-file simple-dir)))
      (error "File doesn't exist")
    (save-window-excursion
      (with-current-buffer
          (find-file (expand-file-name org-file simple-dir))
        (and
         (org-check-for-org-mode)
         (org-export-to-file
          'e-html
          (expand-file-name
           (concat
            (file-name-sans-extension
             (file-name-nondirectory org-file))
            ".html") simple-dir )))))))


(defun iorg-404-handler (httpcon)
  ;; TODO: This should probably actually serve a 404 page rather than
  ;;       throwing an error
  (progn
    (elnode-log-access "simple" httpcon)
    (error "iorg: 404 handler invoked")))

(provide 'simple)
