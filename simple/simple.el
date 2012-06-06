;;; simple.el -- iorg proof of concept
(require 'org)
(require 'elnode)
(require 'org-export)
(require 'org-agenda)
(require 'util)

;;;; Declare functions
(declare-function org-entry-is-todo-p "org" nil)
(declare-function org-get-todo-state "org" nil)
(declare-function org-check-for-org-mode "org-agenda" nil)

(add-to-list 'org-export-filter-final-output-functions
             'iorg-html-postprocess)

(defvar iorg-html-postprocess-begin-txt-regexp
  "<div class=\"outline-text.*\">"
  "Match the beginning of outline text in exported html.")

(defvar iorg-html-postprocess-todo-regexp
  "\\(<span class=\"todo \\)\\([A-Z]+\\)\\(\">\\)"
  "Match todo items in exported html.")

(defconst simple-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The project directory of the 'simple' app in canonical form")

(defconst simple-urls
  '(("^$"      . iorg-initialize-simple-handler)
    ("^todo/$" . iorg-change-state-handler)))

(defun simple-dispatcher-handler (httpcon)
  "Dispatch requests to the 'simple' app"
  (elnode-log-access "*iorg-log*" httpcon)
  (elnode-dispatcher httpcon simple-urls iorg-404-handler))

(defun iorg-html-postprocess (transc-str back-end comm-chan)
  "Add buttons to HTML export to make headlines editable."
  ;; TODO: (2) adding buttons to html export
  (with-temp-buffer 
    (insert transc-str)
    (goto-char (point-min))
    (while (and
            (re-search-forward iorg-html-postprocess-todo-regexp nil t)
            (re-search-forward iorg-html-postprocess-begin-txt-regexp nil t))
      (goto-char (match-beginning 0))
      (insert
       (concat
        "<form action=\"http://localhost:8029/todo/\">"
        "  <input type=\"submit\" value=\" Finish \" name=\" outline-1\">"
        "</form>")))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun iorg-launch (port)
  "Launch the elnode server which will serve and edit simple.org."
  ;; TODO: (1) elnode serving simple.org to html
  (interactive "nPort number: ")
  (elnode-start 'iorg-initialize-simple-handler
                :port port :host "localhost"))

(defun iorg-initialize-simple-handler (httpcon)
  "Serves the start-page of the 'simple' app"
  (elnode-log-access (get-buffer-create "*iorg-log*") httpcon)
  (elnode-send-file httpcon (iorg--org-to-html "simple.org")))

(defun iorg-change-state-handler (httpcon)
  "Called by the elnode form handler to update task state."
  ;; TODO: (3) handle form post data and update an Org-mode file
  (elnode-log-access (get-buffer-create "*iorg-log*") httpcon)
  (message "entering `iorg-change-state-handler'")
  (let ((params (elnode-http-params httpcon)))
    (message "These are the http-params: \n %s" params)))

(defun iorg--org-to-html (org-file)
  "Export ORG-FILE to html and return the expanded filename"
  (if (not (file-exists-p (expand-file-name org-file simple-dir)))
      (error "File doesn't exist")
    (save-window-excursion
      (with-current-buffer (find-file (expand-file-name org-file simple-dir))
        (org-export-to-file
         'e-html
         (expand-file-name
          (concat
           (file-name-sans-extension
            (file-name-nondirectory org-file))
           ".html") simple-dir ))))))

(defun iorg-404-handler (httpcon)
  ;; TODO: This should probably actually serve a 404 page rather than
  ;;       throwing an error
  (elnode-log-access (get-buffer-create "*iorg-log*") httpcon)
  (error "iorg: 404 handler invoked"))

(provide 'simple)
