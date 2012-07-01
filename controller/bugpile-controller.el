;;; bugpile-controller -- elnode handlers and such for handling web
;;; POST/GET requests


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

(provide 'bugpile-controller)
