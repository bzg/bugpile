;;; simple.el -- iorg proof of concept
(require 'org)
(require 'elnode)
(require 'org-export)

;;;; Declare functions
(declare-function org-entry-is-todo-p "org" nil)
(declare-function org-get-todo-state "org" nil)

(add-to-list 'org-export-filter-final-output-functions
             'iorg-html-postprocess)

(defvar iorg-html-postprocess-begin-txt-regexp
  "<div class=\"outline-text.*\">"
  "Match the beginning of outline text in exported html.")

(defvar iorg-html-postprocess-todo-regexp
  "\\(<span class=\"todo \\)\\([A-Z]+\\)\\(\">\\)"
  "Match todo items in exported html.")

(defun iorg-html-postprocess (str back chan)
  "Add buttons to HTML export to make headlines editable."
  ;; TODO: (2) adding buttons to html export
  (let ((transc-str str)
        (back-end back)
        (comm-chan chan))
  (with-temp-buffer 
    (insert transc-str)
    (goto-char (point-min))
    (while
        (and
         (re-search-forward iorg-html-postprocess-todo-regexp nil t)
         (re-search-forward iorg-html-postprocess-begin-txt-regexp nil t))
      (goto-char (match-beginning 0))
      (insert
       "<form action=\"http://localhost:8028/todo/\">
                      <input type=\"submit\" value=\" Finish \">
              </form>"))
    (buffer-substring-no-properties (point-min) (point-max)))))


(defun iorg-launch (port)
  "Launch the elnode server which will serve and edit simple.org."
  ;; TODO: (1) elnode serving simple.org to html
  
  )

(defun iorg-change-state (file headline new-state)
  "Called by the elnode form handler to update task state."
  ;; TODO: (3) handle form post data and update an Org-mode file
  )
