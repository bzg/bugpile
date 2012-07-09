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
  (message "dispatcher-handler httpcon: %s" httpcon)
  (elnode-dispatcher httpcon
                     (iorg-projects-get-project-urls "bugpile")))

(defun bugpile-controller-index-handler (httpcon)
  "Serve the start-page of the Bugpile application"
  (elnode-log-access "bugpile-controller" httpcon)
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                       "bugpile-view-index.org"))))


(defun bugpile-controller-open-new-task-handler (httpcon)
  "Serve a form page for a new task and process user input."
  (elnode-log-access "bugpile-controller" httpcon)  
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
  (message "show-task-handler httpcon: %s" httpcon)
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                       "bugpile-view-show-task.org"))))



(defun bugpile-controller-edit-task-handler (httpcon)
  "Serve a form page for editing the data of an existing task,
process user edits, and present the modified task to the user."
  (elnode-log-access "bugpile-controller" httpcon)  
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                       "bugpile-view-edit-task.org"))))



(defun bugpile-controller-search-tasklist-handler (httpcon)
  "Serve a form page for query composition, process the user
query and present the result list."
  (elnode-log-access "bugpile-controller" httpcon)
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                       "bugpile-view-search-tasklist.org"))))


(defun bugpile-controller-take-action-on-selected-tasks-handler (httpcon)
  "Perform the action chosen by the user on all selected tasks."
  (elnode-log-access "bugpile-controller" httpcon)
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                       "bugpile-view-take-action-on-selected-tasks.org"))))


(provide 'bugpile-controller)
