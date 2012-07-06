;;;; iorg-controller -- elnode handlers and such for handling web POST/GET requests

;;;; Requirements

(require 'org)
(require 'elnode)
(require 'org-export)
(require 'org-e-html)
;;(require 'org-agenda)
(require 'iorg-util)
(require 'iorg-projects)
(require 'iorg-html)
(require 'iorg-logic)

(eval-when-compile
  (require 'cl))


;;;; Variables

;;; Consts

(defconst iorg-controller-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory of iorg-controller.el in canonical form")

;;; Vars

;;; Customs
(defgroup iorg nil
  "A webframework based on Org-mode, Elnode and dVCS."
  :tag "iOrg"
  :group 'org
  :group 'elnode)

(defgroup iorg-controller nil
  "Elnode handlers for handling web request."
  :tag "iorg-controller"
  :group 'iorg)

(defcustom iorg-controller-load-hook nil
  "Hook that is run after iorg-controller.el has been loaded."
  :group 'iorg-controller
  :type 'hook)


;;;; Functions

;;; Function Declarations
(declare-function org-entry-is-todo-p "org" nil)
(declare-function org-get-todo-state "org" nil)
(declare-function org-check-for-org-mode "org-agenda" nil)
(declare-function
 iorg-projects-get-project-info "iorg-projects" nil)
(declare-function iorg-util-goto-first-entry "iorg-util" nil)

;;; Helper Functions

(defun iorg-controller--get-outline-level (param-list)
  "Return level of outline-tree encoded in http-params"
  (and
   (listp param-list)
   (assoc-re iorg-alist-outline-regexp param-list 2)))


(defun iorg-controller--normalize-outline-level (outline-level)
  "Normalize OUTLINE-LEVEL in the format \"[-[:digit:]]+\" to a
list of numbers (as strings). The lenght of the returned list is
equal to the number of sublevels we need to walk down in the
outline tree, the value of each number identifies the nth-entry
in the Org file on that level."
(if (not
     (and
      (non-empty-string-p outline-level)
      (string-match "[-[:digit:]]+" outline-level)))
    (error "Wrong type or format of OUTLINE-LEVEL argument")
  (delete "" (split-string outline-level "-"))))

 (defun iorg-controller--params-find-entry (param-list &optional file)
  "Go to the entry in the current Org buffer that is specified in the PARAM-LIST"
  (condition-case err
      (let* ((outline-level
              (iorg-controller--get-outline-level param-list))
             (normalized-outline-level
              (iorg-controller--normalize-outline-level outline-level))
             (sublevel-p nil))
        (with-current-buffer
            (if (and file (file-exists-p file))
                (find-file file)
             (find-file (expand-file-name "simple.org" iorg-controller-dir)))
          (org-check-for-org-mode)
          (save-restriction
            (widen)
            (iorg-util-goto-first-entry)
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

(defun iorg-controller--org-to-html (org-file)
  "Export ORG-FILE to html and return the expanded filename.
ORG-FILE is given as absolute file-name"
  (if (not (file-exists-p org-file))
      (error "File doesn't exist")
    (let* ((org-file-nondir-sans-ext
            (file-name-nondirectory
             (file-name-sans-extension org-file)))
           (org-file-dir
            (file-name-directory org-file))
           (dir-files
            (directory-files org-file-dir))
           (html-file-nondir
            (concat org-file-nondir-sans-ext ".html"))
           (html-file
            (expand-file-name html-file-nondir org-file-dir)))           
      (if (and (member html-file-nondir dir-files)
               (not (file-newer-than-file-p org-file html-file))                
               html-file)
          (save-window-excursion
            (with-current-buffer (find-file org-file)
              (and
               (org-check-for-org-mode)
               (org-export-to-file
                ;; TODO replace e-html with iorg
                'e-html
                html-file)
               ;; TODO Erics solution
               (kill-buffer (find-file org-file))))
            html-file)))))


(defun iorg-controller--serve-docroot (project proj-config &rest args)
  "Make a webserver serving static files in projects's docroot.

Use data from the alists PROJ-CONFIG or ARGS to configure the
server."
  ;; make sure all .org files in docroot have been exported to html
  ;; and the html files are up-to-date
  (let* ((docroot-dir
          (iorg-projects-get-project-info project :docroot))
         (docroot-files
          (directory-files docroot-dir)))
    (mapc
     (lambda (file)
       (and (string= (file-name-extension file) "org")
            (let ((absolute-file-name
                   (expand-file-name
                    file docroot-dir)))
              (unless
                  (and 
                   (member
                    (concat
                     (file-name-sans-extension file) ".html") docroot-files)
                   (not (file-newer-than-file-p
                         (concat (file-name-sans-extension file) ".org")
                         (concat (file-name-sans-extension file) ".html"))))  
                (with-current-buffer
                    (find-file absolute-file-name)
                  (org-export-to-file
                   'e-html
                   (concat
                    (file-name-sans-extension absolute-file-name)
                    ".html"))
                  ;; TODO safer solution, see Erics mail
                  (kill-buffer (find-file absolute-file-name)))))))
     docroot-files))
  ;; define the webserver handler
  (set (intern
        (concat
         project "-"
         (if (and args (assoc :controller args))
             (cdr (assoc :controller args))
           (cdr (assoc :controller proj-config)))
         "-docroot-handler"))
       (elnode-webserver-handler-maker
        (iorg-projects-get-project-info project :docroot)))
  ;; start an elnode server that serves all the static html files
  ;; in projects docroot 
  (eval
   `(elnode-start
     ,(intern-soft
       (concat
        project "-"
        (if (and args (assoc :controller args))
            (cdr (assoc :controller args))
          (cdr (assoc :controller proj-config)))
        "-docroot-handler"))
     :port (if (and args (assoc :docroot-port args))
               (string-to-int (cdr (assoc :docroot-port args)))
             (string-to-int (cdr (assoc :docroot-port proj-config))))
     :host  (if (and args (assoc :host args))
                (cdr (assoc :host args))
              (cdr (assoc :host proj-config))))))


;;; Public Functions (interactive)

(defun iorg-controller-launch-project
  (project &optional host port &rest args)
  "Launch the elnode server which will serve PROJECT.

PROJECT must be a predefined project in customisation variable
'iorg-projects-config', from which configuration is read. 

If optional arguments HOST and PORT are given, they override the
values from 'iorg-projects-config'.

ARGS is an alist of additional project configuration variables
in the ((:key1 . value1) (:key2 . value2)) format that override
their counterparts in 'iorg-projects-config'"

  (interactive "sProject: ")
  (if (not (and
            (non-empty-string-p project)
            (assoc project iorg-projects-config)))
      (message "%s"
               (concat "Project not registered in customizable "
                       "variable 'iorg-projects-config'"))
    ;; TODO check if (cdr (assoc ...)) is needed
    (let ((proj-config (assoc project iorg-projects-config)))
      (elnode-start
       'bugpile-controller-dispatcher-handler
       :host (or host (cdr (assoc :host proj-config)))
       :port (string-to-int
              (or port (cdr (assoc :port proj-config)))))
      (if args
          (iorg-controller--serve-docroot project proj-config args)
        (iorg-controller--serve-docroot project proj-config)))))


(defun iorg-controller-stop-project-servers (project)
  "Stop both elnode servers started when PROJECT was launched.

Each project is started with one elnode server handling the
http-requests during user interaction, and a second elnode server
serving all static html files in PROJECTs docroot. Host and port
for both servers are defined in 'iorg-projects-config'. This
function looks up the project information and stops both
servers."
  (interactive "sProject: ")
  (if (not (and
            (non-empty-string-p project)
            (assoc project iorg-projects-config)))
      (message "%s"
               (concat "Project not registered in customizable "
                       "variable 'iorg-projects-config'"))
    (let ((proj-config (assoc project iorg-projects-config)))
      (elnode-stop
       (string-to-int
        (cdr (assoc :port proj-config))))
      (elnode-stop
       (string-to-int
        (cdr (assoc :docroot-port proj-config)))))))

;;; Public Functions (non-interactive)              


;;; Public Functions (obsolete?)              

;; (defun iorg-change-state-handler (httpcon)
;;   "Called by the elnode form handler to update task state."
;;   ;; TODO: (3) handle form post data and update an Org-mode file
;;   (message "entering `iorg-change-state-handler'")  
;;   (let ((params (elnode-http-params httpcon)))
;;     (message "These are the http-params: \n %s" params)
;;     (with-current-buffer
;;         (find-file (expand-file-name "simple.org" iorg-controller-dir))
;;       (save-excursion
;;         (iorg-controller--params-find-entry params)
;;         (org-todo 'done))
;;       (save-buffer)
;;       ;(kill-buffer (current-buffer))
;;       )
;;     (iorg-initialize-iorg-controller-handler httpcon)))


;; (defun iorg-edit-headline-handler (httpcon)
;;   "Called by the elnode form handler to update headline."
;;   ;; TODO: (3) handle form post data and update an Org-mode file
;;   (message "entering `iorg-edit-headline-handler'")  
;;   (let ((params (elnode-http-params httpcon)))
;;     (message "These are the http-params: \n %s" params)
;;     (with-current-buffer
;;         (find-file (expand-file-name "simple.org" iorg-controller-dir))
;;       (save-excursion
;;         ;; (iorg-controller--params-find-entry params)
;;         ;; (org-todo 'done))
;;       (save-buffer)
;;       ;(kill-buffer (current-buffer))
;;       )
;;     (iorg-initialize-iorg-controller-handler httpcon))))
  
;; (defun iorg-controller-postprocess (transc-str back-end comm-chan)
;;   "Add buttons to HTML export to make headlines editable."
;;   ;; TODO: (2) adding buttons to html export
;;   (with-temp-buffer
;;     (insert transc-str)
;;     (goto-char (point-min))
;;     (while (and
;;             (re-search-forward iorg-controller-todo-regexp nil t)
;;             (re-search-forward iorg-controller-outline-text-regexp nil t))
;;       (goto-char (match-beginning 0))
;;       (insert
;;        (concat
;;         "<form action=\"http://localhost:8031/todo/\">"
;;         "  <input type=\"submit\" value=\" Finish \" name=\"outline-1\">"
;;         "</form>")))
;;     (buffer-substring-no-properties (point-min) (point-max))))



;; (defun iorg-404-handler (httpcon)
;;   ;; TODO: This should probably actually serve a 404 page rather than
;;   ;;       throwing an error
;;   (progn
;;     (elnode-log-access "simple" httpcon)
;;     (error "iorg: 404 handler invoked")))

;; (defun iorg-controller-static-export-handler (httpcon file)
;;   "Exports an Org file to static html"
;;   (elnode-send-file httpcon (iorg-controller--org-to-html file 'STATIC)))

(provide 'iorg-controller)
