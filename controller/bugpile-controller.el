;;;; bugpile-controller -- elnode handlers and such for handling web
;;;; POST/GET requests

;;;; Requirements
(require 'elnode)
(require 'iorg-controller)
(require 'iorg-projects)



;;;; Variables

;;; Consts

;;; Vars
;; (defvar bugpile-controller-docroot-handler nil)

;;; Customs



;;;; Functions

;;; Function Declarations

;;; Helper Functions

;;; Public Functions (interactive)

;;; Public Functions (non-interactive)              

(defun bugpile-controller-index-handler (httpcon)
  "Serves the start-page of the Bugpile application"
  (elnode-send-file httpcon
                    (iorg-controller--org-to-html
                     (concat
                      (iorg-projects--get-project-info
                       "bugpile" :view)
                       "bugpile-index.org"))))

(defun bugpile-controller-dispatcher-handler (httpcon)
  "Dispatch requests to the Bugpile application."
  (elnode-log-access "bugpile-controller" httpcon)
  (elnode-dispatcher httpcon
                     (iorg-projects--get-project-urls "bugpile")))



(provide 'bugpile-controller)
