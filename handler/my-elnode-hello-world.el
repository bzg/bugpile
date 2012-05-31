(defun my-elnode-hello-world-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return
   httpcon
   "<html><body><h1>Hello World</h1></body></html>"))

(elnode-start 'my-elnode-hello-world-handler :port 8028 :host "localhost")

                
