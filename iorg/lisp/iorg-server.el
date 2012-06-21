;;; iorg-server -- elnode handlers and such for handling web POST/GET requests

(require 'org)
(require 'elnode)
(require 'org-export)
(require 'org-e-html)
(require 'org-agenda)
(require 'iorg-util)

(eval-when-compile
  (require 'cl))


;;; Customs, Constants and Variables

;; Customisation Groups and Variables
(defgroup iorg nil
  "A webframework based on Org-mode, Elnode and dVCS."
  :tag "iOrg"
  :group 'org
  :group 'elnode)

(defgroup iorg-server nil
  "Elnode handlers for handling web request."
  :tag "iOrg-Server"
  :group 'iorg)

(defcustom iorg-server-load-hook nil
  "Hook that is run after iorg-server.el has been loaded."
  :group 'iorg-server
  :type 'hook)

;; Constants
(defconst iorg-server-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory of iorg-server.el in canonical form")

(defconst iorg-server-urls
  '(("^$"      . iorg-initialize-simple-handler)
    ("^edit/$" . iorg-change-state-handler)
    ("^send/$" . iorg-change-state-handler)
    ("^reset/$" . iorg-edit-headline-handler)))

;; Variables


;;; Functions

;; Declare functions
(declare-function org-entry-is-todo-p "org" nil)
(declare-function org-get-todo-state "org" nil)
(declare-function org-check-for-org-mode "org-agenda" nil)



(defun iorg-server-dispatcher-handler (httpcon)
  "Dispatch requests to the 'iorg-server' app"
  (elnode-log-access "iorg-server" httpcon)
  (elnode-dispatcher httpcon iorg-server-urls))

(defun iorg-server-postprocess (transc-str back-end comm-chan)
  "Add buttons to HTML export to make headlines editable."
  ;; TODO: (2) adding buttons to html export
  (with-temp-buffer
    (insert transc-str)
    (goto-char (point-min))
    (while (and
            (re-search-forward iorg-server-todo-regexp nil t)
            (re-search-forward iorg-server-outline-text-regexp nil t))
      (goto-char (match-beginning 0))
      (insert
       (concat
        "<form action=\"http://localhost:8031/todo/\">"
        "  <input type=\"submit\" value=\" Finish \" name=\"outline-1\">"
        "</form>")))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun* iorg-server-launch (project &allow-other-keys)
  "Launch the elnode server which will serve PROJECT.

PROJECT must be a predefined project in customisation variable
'iorg-projects-config', from which configuration is read. 

An alist of additional project configuration variables in
the ((:key1 . value1) (:key2 value2)...) format can be given as
other keys that override their counterparts in
'iorg-projects-config'. Any keys that do not have a counterpart
in 'iorg-projects-config' are simply ignored."

  (interactive "DProject: ")
  ;; (apply
  ;;  (lambda (key)
  ;;    (and (member key iorg-projects-config)

  (message  "%s" (apply cdr other-keys)))

          ;; (elnode-start
          ;;  'iorg-server-dispatcher-handler
          ;;  :host proj-host :port proj-port :docroot proj-docroot))

(defun iorg-initialize-iorg-server-handler (httpcon)
  "Serves the start-page of the 'simple' app"
  (elnode-send-file httpcon (iorg--org-to-html "simple.org")))

 (defun iorg-change-state-handler (httpcon)
  "Called by the elnode form handler to update task state."
  ;; TODO: (3) handle form post data and update an Org-mode file
  (message "entering `iorg-change-state-handler'")  
  (let ((params (elnode-http-params httpcon)))
    (message "These are the http-params: \n %s" params)
    (with-current-buffer
        (find-file (expand-file-name "simple.org" iorg-server-dir))
      (save-excursion
        (iorg--params-find-entry params)
        (org-todo 'done))
      (save-buffer)
      ;(kill-buffer (current-buffer))
      )
    (iorg-initialize-iorg-server-handler httpcon)))


;; (defun iorg-edit-headline-handler (httpcon)
;;   "Called by the elnode form handler to update headline."
;;   ;; TODO: (3) handle form post data and update an Org-mode file
;;   (message "entering `iorg-edit-headline-handler'")  
;;   (let ((params (elnode-http-params httpcon)))
;;     (message "These are the http-params: \n %s" params)
;;     (with-current-buffer
;;         (find-file (expand-file-name "simple.org" iorg-server-dir))
;;       (save-excursion
;;         ;; (iorg--params-find-entry params)
;;         ;; (org-todo 'done))
;;       (save-buffer)
;;       ;(kill-buffer (current-buffer))
;;       )
;;     (iorg-initialize-iorg-server-handler httpcon))))
  

(defun iorg--get-outline-level (param-list)
  "Return level of outline-tree encoded in http-params"
  (and
   (listp param-list)
   (assoc-re iorg-alist-outline-regexp param-list 2)))


(defun iorg--normalize-outline-level (outline-level)
  "Normalize OUTLINE-LEVEL in the format \"[-[:digit:]]+\" to a
list of numbers (as strings). The lenght of the returned list is
equal to the number of sublevels we need to walk down in the
outline tree, the value of each number identifies the nth-entry
in the Org file on that level."
(if (not
     (and
      (iorg--stringp outline-level)
      (string-match "[-[:digit:]]+" outline-level)))
    (error "Wrong type or format of OUTLINE-LEVEL argument")
  (delete "" (split-string outline-level "-"))))

 (defun iorg--params-find-entry (param-list &optional file)
  "Go to the entry in the current Org buffer that is specified in the PARAM-LIST"
  (condition-case err
      (let* ((outline-level
              (iorg--get-outline-level param-list))
             (normalized-outline-level
              (iorg--normalize-outline-level outline-level))
             (sublevel-p nil))
        (with-current-buffer
            (if (and file (file-exists-p file))
                (find-file file)
             (find-file (expand-file-name "simple.org" iorg-server-dir)))
          (org-check-for-org-mode)
          (save-restriction
            (widen)
            (iorg--goto-first-entry)
            (mapc
             (lambda (n)
               (if sublevel-p
                   (outline-next-heading))
               (org-forward-same-level
                (1- (string-to-number n)) "INVISIBLE-OK")
               (unless sublevel-p
                 (setq sublevel-p 1)))
             normalized-outline-level))))
    (error "Error while going to outline entry specified in PARAM-LIST: %s " err)))

(defun iorg--org-to-html (org-file)
  "Export ORG-FILE to html and return the expanded filename"
  (if (not (file-exists-p (expand-file-name org-file iorg-server-dir)))
      (error "File doesn't exist")
    (save-window-excursion
      (with-current-buffer
          (find-file (expand-file-name org-file iorg-server-dir))
        (and
         (org-check-for-org-mode)
         (org-export-to-file
          'e-html
          (expand-file-name
           (concat
            (file-name-sans-extension
             (file-name-nondirectory org-file))
            ".html") iorg-server-dir )))))))


(defun iorg-404-handler (httpcon)
  ;; TODO: This should probably actually serve a 404 page rather than
  ;;       throwing an error
  (progn
    (elnode-log-access "simple" httpcon)
    (error "iorg: 404 handler invoked")))

(provide 'iorg-server)




; -----------------------------------


(defun iorg-server-static-export-handler (httpcon file)
  "Exports an Org file to static html"
  (elnode-send-file httpcon (iorg--org-to-html file 'STATIC)))


(defun iorg--org-to-html (org-file &optional STATIC)
  "Export ORG-FILE to html and return the expanded filename. If STATIC is non nil, export to static html, otherwise use the iOrg exporter"
  (if (not (file-exists-p (expand-file-name org-file iorg-server-dir)))
      (error "File doesn't exist")
    (save-window-excursion
      (with-current-buffer
          (find-file (expand-file-name org-file iorg-server-dir))
        (and
         (org-check-for-org-mode)
         (org-export-to-file
          (if STATIC 'e-html 'iorg) 
          (expand-file-name
           (concat
            (file-name-sans-extension
             (file-name-nondirectory org-file))
            ".html") iorg-server-dir )))))))




(provide 'iorg-server)
