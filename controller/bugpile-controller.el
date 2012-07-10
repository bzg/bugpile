;;;; bugpile-controller -- elnode handlers and such for handling web
;;;; POST/GET requests

;;;; Requirements
(require 'iorg-controller)

;;;; Variables

;;; Consts

;;; Vars

;;; Customs



;;;; Functions

;;; Function Declarations

;;; Helper Functions

;;; Public Functions (interactive)

;;; Public Functions (non-interactive)              

(defun bugpile-controller-dispatcher-handler (httpcon)
  "Dispatch requests to the Bugpile application."
  (elnode-log-access "bugpile-controller" httpcon)
  (message "dispatcher elnode-params: %s"
           (elnode-http-params httpcon))
  (elnode-dispatcher httpcon
                     (iorg-projects-get-project-urls "bugpile")))

(defun bugpile-controller-index-handler (httpcon)
  "Serve the start-page of the Bugpile application"
  (elnode-log-access "bugpile-controller" httpcon)
  (message "index elnode-params: %s"
           (elnode-http-params httpcon))
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                      "bugpile-view-index.org"))))


(defun bugpile-controller-open-new-task-handler (httpcon)
  "Serve a form page for a new task and process user input."
  (elnode-log-access "bugpile-controller" httpcon)
  (message "open-new-task elnode-params: %s"
           (elnode-http-params httpcon))
  ;; (elnode-send-file httpcon
  ;;                   (iorg-controller--org-to-html
  ;;                    (concat
  ;;                     (iorg-projects-get-project-info
  ;;                      "bugpile" :view)
  ;;                      "bugpile-index.org"))))
  )

(defun bugpile-controller-show-task-handler (httpcon)
  "Serve a page with edit button that shows an existing task."
  (elnode-log-access "bugpile-controller" httpcon)
  (message "show-task elnode-params: %s"
           (elnode-http-params httpcon))
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                      "bugpile-view-show-task.org"))))

(defun bugpile-controller-search-tasklist-handler (httpcon)
  "Serve a form that enables query compositon."
  (elnode-log-access "bugpile-controller" httpcon)
  (message "search-tasklist elnode-params: %s"
           (elnode-http-params httpcon))
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                      "bugpile-view-search-tasklist.org"))))


(defun bugpile-controller-edit-task-handler (httpcon)
  "Serve a form page for editing the data of an existing task,
process user edits, and present the modified task to the user."
  (elnode-log-access "bugpile-controller" httpcon)  
  (message "edit-task elnode-params: %s"
           (elnode-http-params httpcon))
  (let ((view-id "3675e953-7f75-4319-a1e5-dfb09cadea1f")
        (obj-id "2f822a1e-4bb4-43be-bec4-b0c5caaa42a5"))
    (save-excursion
      (save-restriction
        (org-id-goto obj-id)
        (org-check-for-org-mode)
        (widen)
        ;; (unless
        ;;     (member "iorg" (org-get-tags))
        ;;   (org-set-tags-to
        ;;  (cons "iorg" (org-get-tags)))
        (org-copy-subtree)
        (org-id-goto view-id)
        (org-check-for-org-mode)
        (widen)
        (show-all)
        (iorg-util-goto-last-entry)
        (when (member "anchor" (org-get-tags))
          (goto-char
           (org-entry-end-position))
          (newline)
          (yank)
          (save-buffer)))))        
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                      "bugpile-view-edit-task.org"))))

(defun bugpile-controller-save-edits-handler (httpcon)
  "Save the modified object data."
  (elnode-log-access "bugpile-controller" httpcon)
  (message "save-edits elnode-params: %s"
           (elnode-http-params httpcon))
  (let ((view-id "3675e953-7f75-4319-a1e5-dfb09cadea1f")
        (obj-id "2f822a1e-4bb4-43be-bec4-b0c5caaa42a5")
        (prop (elnode-http-param httpcon 'simple-prop))
        (tag (elnode-http-param httpcon 'simple-tag))
        (section (elnode-http-param httpcon 'simple-section)))
    (message "prop: %s \ntag: %s \nsection: %s" prop tag section)
    (save-excursion
      (save-restriction
        ;; goto object file
        (org-id-goto obj-id)
        (org-check-for-org-mode)
        (widen)
        (show-all)
        ;; set property
        (org-entry-put (point)
        "object-foo" (cdr prop))
        ;; set tags
        (org-set-tags-to (cdr tag))
        ;; set content ??
        (save-buffer)
        ;; goto view file
        (org-id-goto view-id)
        (org-check-for-org-mode)
        (widen)
        (show-all)
        (iorg-util-goto-last-entry)
        ;; remove iorg tag
        (unless (member "anchor" (org-get-tags))
          (org-set-tags-to
           (remove "iorg" (org-get-tags)))
          (save-buffer)))))
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                      "bugpile-view-edit-task.org"))))


(defun bugpile-controller-take-action-on-selected-tasks-handler (httpcon)
  "Perform the action chosen by the user on all selected tasks."
  (elnode-log-access "bugpile-controller" httpcon)
  (message "take-action elnode-params: %s"
           (elnode-http-params httpcon))
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                      "bugpile-view-take-action-on-selected-tasks.org"))))


(provide 'bugpile-controller)
