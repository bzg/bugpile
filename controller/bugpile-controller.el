;;; bugpile-controller -- elnode handlers and such for handling web
;;; POST/GET requests

(require 'iorg-controller)

;; (require 'iorg-projects)  ; cannot open?

;; (defvar bugpile-controller-docroot-handler nil)

;; TODO abstract and move back to iorg-controller
;; use intern and inter-soft
(defun bugpile-controller-index-handler (httpcon)
  "Serves the start-page of the Bugpile application"
  (elnode-send-file httpcon
                    (iorg--org-to-html
                     (concat
                      (iorg-projects--get-project-info
                       "bugpile" :view)
                       "bugpile-index.org"))))

;; TODO move urls from iorg to bugpile

(defun bugpile-controller-dispatcher-handler (httpcon)
  "Dispatch requests to the Bugpile application."
  (elnode-log-access "bugpile-controller" httpcon)
  (elnode-dispatcher httpcon
                     (iorg-projects--get-project-urls "bugpile")))


(provide 'bugpile-controller)
