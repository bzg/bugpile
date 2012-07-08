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
  (elnode-dispatcher httpcon
                     (iorg-projects-get-project-urls "bugpile")))

(defun bugpile-controller-index-handler (httpcon)
  "Serves the start-page of the Bugpile application"
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects-get-project-info
                       "bugpile" :view)
                       "bugpile-index.org"))))


(defun bugpile-controller-open-new-task-handler (httpcon)
  "Serve a form page for a new task and process user input."
  ;; (elnode-send-file httpcon
  ;;                   (iorg-controller--org-to-html
  ;;                    (concat
  ;;                     (iorg-projects-get-project-info
  ;;                      "bugpile" :view)
  ;;                      "bugpile-index.org"))))
)

(defun bugpile-controller-search-tasklist-handler (httpcon)
  "Serve a form page for query composition, process the user
query and present the result list."
  ;; (elnode-send-file httpcon
  ;;                   (iorg-controller--org-to-html
  ;;                    (concat
  ;;                     (iorg-projects-get-project-info
  ;;                      "bugpile" :view)
  ;;                      "bugpile-index.org"))))
)

(defun bugpile-controller-take-action-on-selected-tasks-handler (httpcon)
  "Perform the action chosen by the user on all selected tasks."
  ;; (elnode-send-file httpcon
  ;;                   (iorg-controller--org-to-html
  ;;                    (concat
  ;;                     (iorg-projects-get-project-info
  ;;                      "bugpile" :view)
  ;;                      "bugpile-index.org"))))
)

(provide 'bugpile-controller)
