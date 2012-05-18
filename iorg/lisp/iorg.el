;;; iorg.el --- A webframework based on Org-mode and Elnode

;; Programming interactive web-applications with Org-mode 
;; Copyright (C) 2012  Thorsten Jolitz 
;; [not yet: Free Software Foundation, Inc.]
;;
;; Author: Thorsten Jolitz <tjolitz at gmail dot com>
;; Co-Author: Eric Schulte  <... at...dot...>
;; Co-Author: Nic Ferrier  <... at...dot...>
;; Keywords: web-applications, interactive, Org-mode, elnode, dvcs
;; Homepage:
;; http://orgmode.org/worg/org-contrib/gsoc2012/student-projects/bugpile/i.org
;;
;; This file is [not yet!] part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; iOrg is a framework for developing web-applications with GNU Emacs
;; Org-mode, using Emacs and its libraries (Org-mode, Elnode, VC, etc)
;; as development environment as well as runtime environment. 
;;
;; iOrg enables declarative programming of dynamic web-applications. For
;; basic functionality, the programmer does not need to write any Emacs 
;; Lisp code, but rather specifies the application within .org files, 
;; using Org-mode syntax and a default directory structure, and then 
;; exports the application and starts the web-server with Org-mode commands. 
;; 
;; For application specific functionality, Emacs Lisp libraries may be added, 
;; with functions that handle the http-requests ('handlers') and functions that
;; implement the actual application logic ('workers'). While the 'handlers' are
;; only useful for web-frontends of iOrg applications, the 'workers' contain 
;; essential application logic independent from the kind of frontend. They are
;; needed too when power users access the application directly via Emacs. 
;;
;; Installation and Activation
;; ---------------------------
;; See the [not yet existing!] iOrg manual at
;;
;;   http://orgmode.org/org.html#iOrg    ???
;;
;; Documentation
;; -------------
;; The documentation of iOrg can be found ????.  The Org-mode
;; distribution also contains a PDF version of it. At Worg, the Org-mode
;; community page, you can read the same text online as HTML. There is also an
;; extended tutorial on Worg, showing how to approach the development of
;; web-applications with iOrg in a systematic way (using a bugtracker as example
;; application). 
;; 
;; A list of recent changes can be found at
;; http://orgmode.org/????
;;
;;; Code:

;;;; Require other packages

(eval-when-compile
  (require 'cl)
;;   (require 'gnus-sum)
  )

(require 'org)
(require 'ob-tangle)
(require 'elnode)
(require 'vc)

;;;; Other stuff we need.

;; remember this directory
(setq iorg-dir
      (car
       (split-string
        (file-name-directory
         (or load-file-name (buffer-file-name))) "lisp/$")))

;; (unless (fboundp 'time-subtract) (defalias 'time-subtract 'subtract-time))

;; (declare-function org-inlinetask-at-task-p "org-inlinetask" ())
;; (declare-function org-inlinetask-outline-regexp "org-inlinetask" ())


;; Customization variables

(defgroup iorg nil
  "A webframework based on Org-mode, Elnode and dVCS."
  :tag "iOrg"
  :group 'org
  :group 'elnode)

(defcustom iorg-mode-hook nil
  "Mode hook for iOrg-mode, run after the mode was turned on."
  :group 'iorg
  :type 'hook)

(defcustom iorg-load-hook nil
  "Hook that is run after iorg.el has been loaded."
  :group 'iorg
  :type 'hook)


;; Initialize new iOrg projects

(defun iorg-initialize-project (dir &optional name)
  "Copy the iOrg project template into DIR and optionally rename the project."
  (interactive "DProject directory: \nsProject name: ")
  (if (file-directory-p dir)
      (let* ((project-name 
              (if (and name (stringp name) (> (length name) 0))
                  name
                (concat
                 "project"
                 (number-to-string
                  (1+ (iorg--max-numbering-unnamed-projects))))))
             (project-dir (concat dir project-name)))
        (copy-directory
         (expand-file-name "project" iorg-dir)
         project-dir)
        (iorg-rename-project project-name project-dir)
        (iorg--update-project project-dir))
    (message "Not a valid directory name")))

(defun iorg--update-project (&optional dir)
  "Update filenames and configuration file for iOrg project in present working directory or DIR."
  (let ((proj
          (if (and dir (file-directory-p dir))
              dir
            (iorg--pwd))))
      (iorg--rename-project-files nil proj)
      ;; (iorg--update-project-config proj)
      ;; (iorg--update-iorg-config proj)
      ))

(defun iorg--pwd ()
  "Return the directory part of the function `pwd'."
  (cadr (split-string (pwd) " ")))


(defun iorg--rename-project-files (&optional name dir)
  "Rename all prefixed files in the present working directory or DIR, replacing the old prefix (taken from <<project>>-config.org) with `file-name-nondirectory' of the project directory."
  (let* ((proj
          (if (and dir (file-directory-p dir))
              dir
            (iorg--pwd)))
         (dir-files (directory-files proj))
         (new-prefix
          (if (and name (stringp name))
              name
            (cdr (iorg--split-dir-name proj))))
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
    (iorg--replace-filename-prefix old-prefix new-prefix proj)
    ;; subdirectories (1 level)
    (mapc
     (lambda (x)
       (unless (string-match-p "^\\.+" x)
         (let ((f (expand-file-name x proj)))
           (and
            (file-directory-p f)
            (iorg--replace-filename-prefix
             old-prefix new-prefix (concat f "/"))))))
     dir-files)))


(defun iorg--replace-filename-prefix (old-prefix new-prefix &optional dir)
  "Replace OLD-PREFIX with NEW-PREFIX in filename of all (non-directory) files in present working directory or DIR."
   (condition-case err
      (let ((proj
             (expand-file-name
             (if (and dir (file-directory-p dir))
                 dir
               (iorg--pwd)))))
        (mapc
         (lambda (x)
           (and
            (string-match
             (concat "\\(^\\)\\(" old-prefix "\\)\\(.+\\)\\($\\)") x)
            (not (file-directory-p
                  (expand-file-name x proj)))               
            (let* ((first-part (match-string 2 x))
                   (last-part (match-string 3 x)))
              (and first-part last-part
                   (rename-file
                    (concat proj first-part last-part)
                    (concat proj new-prefix last-part))))))
         (directory-files proj)))
    ;; error handler
    (error 
     (princ
      (format
       "Error replacing the filename-prefix: %s" err))
     nil)))

(defun iorg--goto-outline-tree-root ()
  "Move point to the beginning of line of the outermost (or first) outline item in the buffer."
  (progn
    (org-goto-line 1)
    (or (looking-at org-outline-regexp)
        (re-search-forward org-outline-regexp-bol nil t))
    (beginning-of-line)))
    

(defun iorg--filter-multival-property (prop reg)
  "Returns a list of strings with all elements of MULTIVAL-PROP that match REGEXP."
  (remove nil 
          (mapcar
           (lambda (x)
             (and (string-match-p reg x) x))
           prop)))

(defun iorg--meta-data ()
  "Return a list with meta-data about the current iOrg projects, gathered from the iorg-config.org file."
  (save-excursion
    (find-file-existing (expand-file-name "iorg-config.org" iorg-dir))
    (iorg--goto-outline-tree-root)
    (let* ((proj (org-entry-get (point) "projects"))
           (unnamed-proj (iorg--filter-multival-property proj "project[0-9]+$"))
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
      ;; kill iorg-config.org buffer
      (kill-buffer (current-buffer))
      ;; build the retpurn list
      (list
       (length proj) ; total number of projects
       (length unnamed-proj) ; number of unnamed projects
       unnamed-max)))) ; highest numbering of unnamed project
       
(defun iorg--number-of-projects ()
  "Return the total number of current iOrg projects."
  (nth 0 (iorg--meta-data)))

(defun iorg--number-of-unnamed-projects ()
  "Return the number of current unnamed iOrg projects."
  (nth 1 (iorg--meta-data)))

(defun iorg--max-numbering-unnamed-projects ()
  "Return the highest numbering of unnamed iOrg projects."
  (nth 2 (iorg--meta-data)))


(defun iorg--project-directory-structure-p (&optional dir)
  "Return t if present working directory or DIR confirms to the iOrg project directory structure, nil otherwise."
  (let* ((project-dir
          (expand-file-name
          (if (and dir (file-directory-p dir))
              dir
            (iorg--pwd))))
         (dir-files (directory-files project-dir)))
    (not
     (cond
      ((not (member
             (concat
              (cadr (iorg--split-dir-name project-dir)) "-config.org")
             dir-files)))
       ((not (member "blob" dir-files)))
       ((not (member "controller" dir-files)))
       ((not (member "dat" dir-files)))
       ((not (member "db" dir-files)))
       ((not (member "img" dir-files)))
       ((not (member "loc" dir-files)))
       ((not (member "log" dir-files)))
       ((not (member "model" dir-files)))
       ((not (member "test" dir-files)))
       ((not (member "view" dir-files)))))))


;; Rename iOrg project

(defun iorg-rename-project (name &optional dir)
  "Rename iOrg project in present working directory of DIR."
  (interactive "sNew project name: \nDProject directory: ")
  (let ((proj
         (if (and dir (file-directory-p dir))
             dir
           (iorg--pwd))))
    (cond
     ((not (iorg--project-directory-structure-p proj))
      (message "Directory does not confirm to iOrg directory structure"))
     ((not (and name (stringp name)))
      (message "New project name is not a valid directory name"))
     (t
      (condition-case err
          (and name proj
               ;; rename the project directory
               (rename-file
                proj
                (concat (car (iorg--split-dir-name proj)) name))
               ;; update the project to the new name
               (iorg--update-project))        
        (error 
         (princ
          (format
           "Error while renaming project: %s" err))
         nil))))))


(defun iorg--split-dir-name (&optional dir)
  "Return as list of strings the base part and the last part of the name of the present working directory or DIR.

The base part is really the name of the parent directory, the last part is the directory name that would appear in the `directory-files' list of the parent directory."
  (let* ((dir-name
          (expand-file-name
           (if (and dir (file-directory-p dir))
               dir
             (iorg--pwd))))
         (split-list
          (split-string dir-name "/"))
         (last-part
          (if (string-equal (car (last split-list)) "")
              (nth (- (length split-list) 2) split-list)
            (car (last split-list))))
         (base-part
          (car (split-string dir-name last-part))))
    
    (list base-part last-part)))  
   

;; Delete iOrg project

(defun iorg-delete-project (&optional project)
  "Delete directory of current project or PROJECT and eliminate it from the `iorg-projects-plist'"
)       
;; Export iOrg project

(defun iorg-export (dir &optional server)
  "Export project defined in the current directory or in DIR, and start the elnode server when SERVER is non-nil"
  (interactive "DProject directory: ")
  (if (file-directory-p dir)
      () ;check dir structure, tangle ob files, export org files
    (message "Not a valid directory name")))
    

  
;; ...
;; Key bindings
;; Documentation
;; Miscellaneous stuff
;; Integration with and fixes for other packages
;; Experimental code
;; Finish up
