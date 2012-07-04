;;;; iorg-projects.el --- functions to create and manage iOrg projects

;;;; Requirements

;;; Require other packages
(require 'ob-tangle)
;; (require 'org)
;; (require 'elnode)
;; (require 'vc)
;; (require 'iorg-util)

;; (eval-when-compile
;;   (require 'cl)
;;   )


;;; Other stuff we need
;; remember this directory
(setq iorg-projects-dir
      (expand-file-name
       (file-name-directory
        (directory-file-name
         (file-name-directory
          (or load-file-name (buffer-file-name)))))))

;; (unless (fboundp 'xyz) (defalias 'xyz 'uvw))



;;;; Variables

;;; Consts

;;; Vars 
  
;; (defvar org-babel-src-block-regexp
;;   (concat
;;    ;; (1) indentation                 (2) lang
;;    "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
;;    ;; (3) switches
;;    "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
;;    ;; (4) header arguments
;;    "\\([^\n]*\\)\n"
;;    ;; (5) body
;;    "\\([^\000]*?\n\\)?[ \t]*#\\+end_src")
;;   "Regexp used to identify code blocks.")

;;; Customs
(defgroup iorg-projects nil
  "Creating and managing iOrg projects"
  :tag "iOrg-Projects"
  :group 'iorg)

(defcustom iorg-projects-load-hook nil
  "Hook that is run after iorg-projects.el has been loaded."
  :group 'org-iorg
  :type 'hook)

(defcustom iorg-projects-elnode-customisation nil
  "Project-related elnode customisations"
  :group 'iorg-projects
  :type 'plist)

;; FIXME make directory names more generic/portable for multi-person
;; projects on multiple platforms
(defcustom iorg-projects-config
  '(("test" . ((:dir . "~/git/test/")
                  (:host . "localhost")
                  (:port . "8088")
                  (:docroot . "docroot")
                  (:docroot-port . "8089")
                  ;; (:docroot-handler . "test-server-docroot-handler")
                  (:logic . "db")
                  (:view . "view") 
                  (:controller . "server")
                  (:objects . "objects")
                  (:test . "test")
                  (:doc . "doc")))
    ("bugpile" . ((:dir . "~/git/bugpile/")
               (:host . "localhost")
               (:port . "8008")
               (:docroot . "docroot")
               (:docroot-port . "8009")
               ;; (:docroot-handler . "bugpile-controller-docroot-handler")
               (:logic . "logic")
               (:view . "view") 
               (:controller . "controller")
               (:objects . "objects")
               (:test . "test")
               (:doc . "doc"))))
  
  "Alist of iOrg projects with configuration. 

The project name is used as a key, project configuration as an
alist for that key. DIR defines the directory of the project,
DOCROOT the webserver root directory, MODEL, VIEW, CONTROLLER the
directories were the related elisp and Org files are stored, TEST
and DOC the directories for tests and documentation and
PERSISTENCE the directory for Org files used as data storage.
Thus, the projects view directory would be defined as
'DIR/VIEW/' (e.g. ~/git/bugpile/view), the projects controller
directory as 'DIR/CONTROLLER/'(e.g. ~/git/bugpile/server/).

HOST and PORT are used to configure the projects elnode webserver
as 'http://HOST:PORT', e.g. 'http://localhost:8008'"

  :group 'iorg-projects
  :type '(alist :key-type string
                :value-type alist))


(defcustom iorg-projects-urls
  '(("bugpile" . (("^$"      . bugpile-controller-index-handler)
                  ("^edit/$" . iorg-controller-edit-handler)
                  ("^send/$" . iorg-controller-send-handler)
                  ("^reset/$" . iorg-controller-reset-handler)
                  ("^open-new-task/$" .
                   bugpile-controller-open-new-task-handler)
                  ("^search-tasklist/$" .
                   bugpile-controller-search-tasklist-handler)
                  ("^take-action-on-selected-tasks/$" .
                   bugpile-controller-take-action-on-selected-tasks-handler)
                  ))
    ("test" .  (("^$"      . test-controller-index-handler)
                  ("^edit/$" . iorg-controller-edit-handler)
                  ("^send/$" . iorg-controller-send-handler)
                  ("^reset/$" . iorg-controller-reset-handler))))


               ;; (("^$" . iorg-initialize-simple-handler)
               ;; ("^edit/$" . iorg-change-state-handler)
               ;; ("^send/$" . iorg-change-state-handler)
               ;; ("^reset/$" . iorg-edit-headline-handler))))

  
  "Alist of iOrg projects with urls. 

The project name is used as a key, project urls as an alist for
that key. Each key in that alist represents an url, the
associated value a function that handles http-requests to that
url."

  :group 'iorg-projects
  :type '(alist :key-type string
                :value-type alist))
 


;;;; Functions

;;; Function Declarations
(declare-function org-check-for-org-mode "org-agenda" nil)
(declare-function org-entry-is-todo-p "org" nil)
(declare-function org-get-todo-state "org" nil)
(declare-function iorg-util-goto-first-entry "iorg-logic" nil)
(declare-function iorg-util-goto-last-entry "iorg-logic" nil)
(declare-function iorg-util-goto-first-entry "iorg-logic" nil)

;;; Private functions (iOrg meta data)

(defun iorg-projects--update-iorg-config (prop val)
  "Update property PROP with value VAL in the global iOrg
configuration file."
  (org-entry-add-to-multivalued-property
   (iorg-util-goto-first-entry
    (expand-file-name "iorg-projects-config.org" iorg-projects-dir)) prop val))

(defun iorg-projects--meta-data ()
  "Return a list with meta-data about the current iOrg projects,
gathered from the iorg-projects-config.org file."
  (iorg-util-goto-first-entry (expand-file-name "iorg-projects-config.org" iorg-projects-dir))
  (let* ((proj (org-entry-get (point) "projects"))
         (unnamed-proj (iorg-util-filter-multival-property proj "project[0-9]+$"))
         (unnamed-max 0))
    ;; get the highest numbering of unnamed projects
    (mapc
     (lambda (x)
       (string-match "(project)([0-9]+)($)" x)
       (let ((proj-count
              (string-to-number
               (match-string 2 x))))
         (and (> proj-count unnamed-max)
              (setq unnamed-max proj-count))))
     unnamed-proj)
    ;; kill iorg-projects-config.org buffer
    (kill-buffer (current-buffer))
    ;; build the retpurn list
    (list
     (length proj) ; total number of projects
     (length unnamed-proj) ; number of unnamed projects
     unnamed-max))) ; highest numbering of unnamed project
       
(defun iorg-projects--number-of-projects ()
  "Return the total number of current iOrg projects."
  (nth 0 (iorg-projects--meta-data)))

(defun iorg-projects--number-of-unnamed-projects ()
  "Return the number of current unnamed iOrg projects."
  (nth 1 (iorg-projects--meta-data)))
 
(defun iorg-projects--max-numbering-unnamed-projects ()
  "Return the highest numbering of unnamed iOrg projects."
  (nth 2 (iorg-projects--meta-data)))

(defun iorg-projects--get-project-info (project key)
  "Return the value of KEY for PROJECT."
  (if (not (and (non-empty-string-p project)
                (assoc project iorg-projects-config)))
      (error (concat "Project not registered in customizable "
                     "variable 'iorg-projects-config'"))
    (cond 
     ((member
       key '(:docroot :logiv :view :controller :objects :doc :test))
      (iorg-util-normalize-existing-dir-name
       (concat
        (iorg-util-normalize-existing-dir-name
         (cdr (assoc :dir (cdr (assoc project iorg-projects-config)))))
        (cdr (assoc key (cdr (assoc project iorg-projects-config)))))))
     ((or (member key '(:dir :host :port))
          (assoc key (cdr (assoc project iorg-projects-config))))
      (cdr (assoc key (cdr (assoc project iorg-projects-config)))))
     (t (error "KEY not found or wrong format - missing leading colon?")))))

(defun iorg-projects--get-project-urls (project)
  "Return the alist of url's and handlers for PROJECT."
  (if (not (and (non-empty-string-p project)
                (assoc project iorg-projects-urls)))
      (error (concat "Project not registered in customizable "
                     "variable 'iorg-projects-urls'"))
    (cdr (assoc project iorg-projects-urls))))

(defun iorg-projects--get-project-url-handler (project url)
  "Return the handler for URL in PROJECT."
  (and (assoc url (iorg-projects--get-project-urls project))
       (cdr (assoc url (iorg-projects--get-project-urls project)))))


;;; Helper Functions (project management)

(defun iorg-projects--update-project-config (prop val &optional dir)
  "Update the iOrg project configuration of project in present
working directory or DIR."
  (org-entry-add-to-multivalued-property
   (iorg-util-goto-first-entry
    (if dir
        (expand-file-name
         (concat
          (file-name-nondirectory
           (directory-file-name
            (iorg-util-normalize-existing-dir-name dir))) "-config.org")
         (iorg-util-normalize-existing-dir-name dir))
      (expand-file-name
       (concat
        (file-name-nondirectory
         (directory-file-name
          (iorg-util-pwd))) "-config.org")
       (iorg-util-pwd)))
    prop val)))

(defun iorg-projects--rename-project-files (&optional name dir)
  "Rename all prefixed files in the present working directory or
DIR, replacing the old prefix (taken from <<project>>-config.org)
with `file-name-nondirectory' of the project directory."
  (let* ((proj
          (if dir
              (iorg-util-normalize-existing-dir-name dir)
            (iorg-util-pwd)))
         (dir-files (directory-files proj))
         (new-prefix
          (if (and name (non-empty-string-p name))
              name
            (file-name-nondirectory (directory-file-name proj))))
         (old-prefix))
    ;; get the old prefix
    (mapc
     (lambda (x)
       (and
        (string-match "\\(^\\)\\(.+\\)\\(-config.org\\)\\($\\)" x)
        (match-string 2 x)
        (setq old-prefix (match-string 2 x))))
     dir-files)
    ;; replace old-prefix with new-prefix
    ;; project directory
    (iorg-util-replace-filename-prefix old-prefix new-prefix proj)
    ;; subdirectories (1st level)
    (mapc
     (lambda (x)
       (unless (string-match-p "^\\.+" x)
         (let ((f (iorg-util-normalize-new-dir-name
                   (concat proj x))))
           (and
            (file-directory-p f)
            (iorg-util-replace-filename-prefix
             old-prefix new-prefix f)))))
     dir-files)))

(defun iorg-projects--project-directory-structure-p (&optional dir)
  "Return t if present working directory or DIR confirms to the
iOrg project directory structure, nil otherwise."
  (let* ((project-dir
          (if dir
              (iorg-util-normalize-existing-dir-name dir)
            (iorg-util-pwd)))
         (dir-files (directory-files project-dir)))
    (not
     (cond
      ((not (member
             (concat
              (file-name-nondirectory
               (directory-file-name project-dir)) "-config.org")
             dir-files)))
      ;; ((not (member "blob" dir-files)))
      ((not (member "controller" dir-files)))
      ;; ((not (member "dat" dir-files)))
      ((not (member "objects" dir-files)))
      ((not (member "img" dir-files)))
      ;; ((not (member "loc" dir-files)))
      ;; ((not (member "log" dir-files)))
      ((not (member "logic" dir-files)))
      ((not (member "test" dir-files)))
      ((not (member "view" dir-files)))))))


;;; Public Functions (interactive - project management)

(defun iorg-projects-initialize-project (&optional dir name)
  "Copy the iOrg project template into DIR and rename the project."
  (interactive "DProject directory: \nsProject name: ")
  (let* ((directory
          (if dir
              (iorg-util-normalize-existing-dir-name dir)
            (iorg-util-pwd)))
         (project-name 
          (if (and name (non-empty-string-p name))
              name
            (concat
             "project"
             (number-to-string
              (1+ (iorg-projects--max-numbering-unnamed-projects))))))
         (project-dir (concat directory project-name)))
    (copy-directory
     (concat
      iorg-projects-dir "project")
     project-dir)
    (iorg-projects-update-project project-dir)))


(defun iorg-projects-update-project (&optional dir)
  "Update filenames and configuration file for iOrg project in
present working directory or DIR."
  (interactive "DProject directory: ")
  (let ((proj
         (if dir
             (iorg-util-normalize-existing-dir-name dir)
           (iorg-util-pwd))))
    (iorg-projects--rename-project-files nil proj)
    ;; (iorg-projects--update-project-config proj)
    ;; (iorg-projects--update-iorg-projects-config proj)
    ))

(defun iorg-projects-rename-project (name &optional dir)
  "Rename iOrg project in present working directory of DIR."
  (interactive "sNew project name: \nDProject directory: ")
  (let ((proj
         (if dir
             (iorg-util-normalize-existing-dir-name dir)
           (iorg-util-pwd))))
    (cond
     ((not (iorg-projects--project-directory-structure-p proj))
      (message "Directory does not confirm to iOrg directory structure."))
     ((not (non-empty-string-p name))
      (message "New project name must be a string of length > 0"))
     (t
      (condition-case err
          ;; rename and update project
          (let ((new-proj
                 (iorg-util-normalize-new-dir-name
                  (concat
                   (file-name-directory
                    (directory-file-name proj)) name))))
            (rename-file
             (directory-file-name proj)
             new-proj)
            (iorg-projects-update-project new-proj))
        (error 
         (princ
          (format
           "Error while renaming project: %s" err))
         nil))))))
 
(defun iorg-projects-delete-project (&optional project)
  "Delete directory of current project or PROJECT and eliminate
  it from the `iorg-projects-projects-plist'")

(defun iorg-projects-export-project (dir &optional server)
  "Export project defined in the current directory or in DIR, and
start the elnode server when SERVER is non-nil"
  (interactive "DProject directory: ")
  (if (file-directory-p dir)
      () ;check dir structure, tangle ob files, export org files
    (message "Not a valid directory name")))


;;; Public Functions (non-interactive)              

(provide 'iorg-projects)
