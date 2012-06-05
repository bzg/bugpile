;;; simple.el -- iorg proof of concept
(require 'org)
(require 'elnode)
(require 'org-export)


;;;; Declare functions
(declare-function org-entry-is-todo-p "org" nil)
(declare-function org-get-todo-state "org" nil)

;; ;; obsolete - old exporter
;; (add-hook 'org-export-html-final-hook 'iorg-html-postprocess)


;; (defvar org-export-filter-final-output-functions nil
;;   "List of functions applied to the transcoded string.
;; Each filter is called with three arguments: the full transcoded
;; string, the back-end, as a symbol, and the communication channel,
;; as a plist.  It must return a string that will be used as the
;; final export output.")

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
    (while (re-search-forward iorg-html-postprocess-todo-regexp)
      (re-search-forward iorg-html-postprocess-begin-txt-regexp)
      (re-search-backward "<div")
      (insert
       "<form action=\"URI\" method=\"Methode\" enctype=\"MIME-Typ\">
<!-- Formularelemente und andere Elemente innerhalb des
Formulars -->
</form>"))
    (buffer-string))))

(defun iorg-launch (port)
  "Launch the elnode server which will serve and edit simple.org."
  ;; TODO: (1) elnode serving simple.org to html
  )

(defun iorg-change-state (file headline new-state)
  "Called by the elnode form handler to update task state."
  ;; TODO: (3) handle form post data and update an Org-mode file
  )
