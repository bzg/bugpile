;;; iorg-controller.el -- generic reusable and use case
;;; independent controller functions for the iOrg framework

(defun iorg-static-export-handler (httpcon file)
  "Exports an Org file to static html"
  (elnode-send-file httpcon (iorg--org-to-html file 'STATIC)))


(defun iorg--org-to-html (org-file &optional STATIC)
  "Export ORG-FILE to html and return the expanded filename. If STATIC is non nil, export to static html, otherwise use the iOrg exporter"
  (if (not (file-exists-p (expand-file-name org-file simple-dir)))
      (error "File doesn't exist")
    (save-window-excursion
      (with-current-buffer
          (find-file (expand-file-name org-file simple-dir))
        (and
         (org-check-for-org-mode)
         (org-export-to-file
          (if STATIC 'e-html 'iorg) 
          (expand-file-name
           (concat
            (file-name-sans-extension
             (file-name-nondirectory org-file))
            ".html") simple-dir )))))))




(provide 'iorg-controller)
