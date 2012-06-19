;;; iorg-server.el --- elnode server for iorg

;;; Code:
(require 'elnode)
(require 'org-e-iorg-js)

(defconst iorg-server-urls
  '(("^$"      . iorg-default-handler)
    ("^edit/$" . iorg-edit-handler)))

(defun iorg-server-dispatcher-handler (httpcon)
  "Dispatch requests to the 'iorg' app"
  (elnode-log-access "iorg" httpcon)
  (elnode-dispatcher httpcon iorg-server-urls))

(defun iorg-default-handler (httpcon)
  "Serves the start-page of the 'simple' app"
  (let ((path ()))
    (elnode-send-file httpcon (file-contents (org-e-iorg-js-export-file path)))))

(defun iorg-edit-handler (httpcon)
  "Called by the elnode form handler to update headline."
  (message "entering `iorg-edit-handler'")  
  (let ((params (elnode-http-params httpcon)))
    (message "These are the http-params: \n %s" params)))

(provide 'iorg-server)
;;; iorg-server.el ends here
