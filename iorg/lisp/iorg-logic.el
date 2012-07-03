;;;; iorg-logic --- generic logic for the iOrg framework. 

;;;; Requirements

;;;; Variables

;;;; Functions

;;; Helper Functions

;;; Public Functions 
(defun iorg-logic-new-object (project class)
  "Instantiate a new object of CLASS in PROJECT.

In the context of iOrg, a class is an Org file with one top-level
entry with a headline and optional todo's and tags. The class
attributes are represented by headline-properties. Each class has
at least one super-class, 'object' is at the top of the hierarchy
and has itself the pseudo-super-class 'root', that doesn't really
exist.

For each non-abstract class (classes tagged as :abstact: can't be
instantiated) there exists one Org file with all the
objects (instances) of that class. There is always one top-level
entry with the class attributes (attributes with the same values
shared by all objects). All objects are stored as second-level
entries, with all instance attributes (with individual values for
each object) from all their superclasses."

  (interactive "sProject: \nfClass: ")
  (cond ((not (and (non-empty-string-p project)
                   (assoc project iorg-projects-config)))
         (message "%s"
                  (concat "Project not registered in customizable "
                          "variable 'iorg-projects-config'")))
        ((not (file-exists-p class))
         (message "Class doesn't exist"))
        (with-current-buffer (find-file class)
          (org-check-for-org-mode)
          (while
              (progn
                ;; do the work
                (iorg-util-goto-first-entry)


                ;; test the condition
                (not
                 (string=
                  (car
                   (org-entry-get-multivalued-property
                    (point) iorg-super))
                  "root")))))))
               

(provide 'iorg-logic)
