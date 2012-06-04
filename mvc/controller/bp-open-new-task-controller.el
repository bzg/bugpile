; url mappings
(defconst bp-open-new-task-urls
  '(("$" . bp-show-open-new-task-handler)
    ("store/.*$" . bp-validate-store-new-task-handler)))

; dispatcher
(defun bp-open-new-task-dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon bp-open-new-task-urls))

; system action
(defun bp-show-open-new-task-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return
   httpcon
   "<html><body><h1>Hello World</h1></body></html>"))

; system action 
(defun bp-validate-store-new-task-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return
   httpcon
   "<html><body><h1>Hello World</h1></body></html>"))

(elnode-start 'bp-open-new-task-dispatcher-handler :port 8028 :host "localhost")

                
