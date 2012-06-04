m;;; simple.el -- iorg proof of concept
(require 'org)
(require 'elnode)

(add-hook 'org-export-html-final-hook 'iorg-html-postprocess)

(defvar iorg-html-postprocess-re "...TODO..."
  "Match headlines in exported html.")

(defun iorg-html-postprocess ()
  "Add buttons to HTML export to make headlines editable."
  ;; TODO: (2) adding buttons to html export
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward iorg-html-postprocess-re))))

(defun iorg-launch (port)
  "Launch the elnode server which will serve and edit simple.org."
  ;; TODO: (1) elnode serving simple.org to html
  )

(defun iorg-change-state (file headline new-state)
  "Called by the elnode form handler to update task state."
  ;; TODO: (3) handle form post data and update an Org-mode file
  )
