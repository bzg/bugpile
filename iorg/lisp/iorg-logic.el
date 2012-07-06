;;;; iorg-logic --- generic logic for the iOrg framework. 

;;;; Requirements

;;;; Variables

;;; Consts

;;; Vars

;;; Customs

(defgroup iorg-logic nil
  "Functionality for working with iOrg classes and objects"
  :tag "iOrg-Logic"
  :group 'iorg)


(defcustom iorg-logic-class-regexp
  "^.+%s-class\\.org$"
  "String to be used by the `format' function when constructing a
class filename regexp."
  :group 'iorg-logic
  :type 'string)

(defcustom iorg-logic-object-regexp
  "^.+%s-obj\\.org$"
  "String to be used by the `format' function when constructing an
object filename regexp."
  :group 'iorg-logic
  :type 'string)

(defcustom iorg-logic-object-filename-tail
  "-obj.org"
  "String that, when encountered as the tail of a filename, identifies the file as iOrg objects file. At least in the context of an iOrgapplication"
  :group 'iorg-logic
  :type 'string)

(defcustom iorg-logic-class-filename-tail
  "-class.org"
  "String that, when encountered as the tail of a filename, identifies the file as iOrg class file. At least in the context of an iOrgapplication."
  :group 'iorg-logic
  :type 'string)


(defcustom iorg-logic-class-headline-format-string
  "#+Title: The class definition for '%s'"
  "String to be used by the `format' function when inserting a
class headline. The format specification '%s' should be replaced
by the name of the class, e.g. 'task'."
  :group 'iorg-logic
  :type 'string)

(defcustom iorg-logic-abstract-class-headline-format-string
  "#+Title: The abstract class definition for '%s'"
  "String to be used by the `format' function when inserting a
abstract class headline. The format specification '%s' should be
replaced by the name of the class, e.g. 'task'"
  :group 'iorg-logic
  :type 'string)

(defcustom iorg-logic-objects-headline-format-string
  "#+Title: The objects file for class '%s'"
  "String to be used by the `format' function when inserting a
headline into an iOrg objects file. The format specification '%s'
should be replaced by the name of the class, e.g. 'task'."
  :group 'iorg-logic
  :type 'string)

(defcustom iorg-logic-class-property-suffix "-C$"
  "Suffix appended to property-keys to indentify them as class properties (with constant values for objects of the class)."
  :group 'iorg-logic
  :type 'string)

(defcustom iorg-logic-class-property-key-regexp
  (lambda ()
    (format "^:?.+%s" iorg-logic-class-property-suffix))
  "Regexp that recognizes property keys in alists that end with
the `iorg-logic-class-property-suffix', identifying them as
class-properties."
  :group 'iorg-logic
  :type 'function)

(defcustom iorg-logic-ignore-tags '("abstract")
  "List of strings that should be ignored when an object is
constructed out of a class hierarchy."
  :group 'iorg-logic
  :type '(repeat string))


(defcustom iorg-logic-ignore-property-keys '("ID")
  "List of strings that should be ignored when an object is
constructed out of a class hierarchy."
  :group 'iorg-logic
  :type '(repeat string))



;;;; Functions

;;; Helper Functions

(defun iorg-logic--postprocess-new-object ()
  "Postprocess new object at point.

Eliminate all tags that are member of the
`iorg-logic-ignore-tags' list as well as all headline properties
whose key is member of the `iorg-logic-ignore-property-keys'
list. Convert all headline properties whose key ends with the
`iorg-logic-class-property-suffix' into file-local-variables."
  (let* ((old-tags (assoc "TAGS" (org-entry-properties)))
         (new-tags
          (remove "" (split-string (cdr old-tags) ":"))))
    ;; delete tags from the ignore list
    (org-set-tags-to 
     (mapconcat
      (lambda (tag)
        (and
         (member tag iorg-logic-ignore-tags)
         (delete tag new-tags)))
      new-tags " "))
    ;; delete properties from the ignore list and convert class
    ;; properties into file-local-variables
    (mapc
     (lambda (association)
       (let ((key (car association))
             (value (cdr association)))
         (and
          (member key iorg-logic-ignore-property-keys)
          (org-entry-delete (point) key))
         (and
          (string-match-p
           iorg-logic-class-property-key-regexp  key)
          (add-file-local-variable key value)
          (org-entry-delete (point) key))))
     (org-entry-properties))))
    
;;; Public Functions (interactive)

;; FIXME interactive for &optional and &rest
;; FIXME class already exists
;; FIXME finally kill Org buffer 
(defun iorg-logic-new-class
  (project name &optional todo &rest tags-and-properties)
  "Make a new class NAME in PROJECT.

In the iOrg context, a class is an Org file in the projects
'classes' directory which is defined under the key :classes in
the customizable variable 'iorg-projects-config'. It has a
headline and one top-level entry. If TODO is non nil, the entry
has a todo state. (Optional) headline tags for the are read from
the TAGS-AND-PROPERTIES, that are associations like (:key .
\"value1 value2\"), with one of them (optionally) defining the
tags (:tags \"tag1\" \"tag2\").

Usage example:
\(iorg-logic-new-class \"bugpile\" \"foo-bug\" 'todo '(:tags
\"foo\" \"doo\") '(:iorg-super-C . \"object html\")
'(:foo-bug-author .\"Bill\"))
               
Each class must have at least two (obligatory) headline
attributes for the 'class' entry, :iorg-super-C: and :ID:, that
are, respectively, read from the TAGS-AND-PROPERTIES
alist (:iorg-super-C . values) or generated by Org mode (with
`org-id-get-create'). Note that class properties are identified
by appending `iorg-logic-class-property-suffix' at the end of
their key. Thus, a typical class might look similar to this:

* TODO <<name>>                   :tags:
  :PROPERTIES:
  :iorg-super-C: html
  :ID:       5bccaee7-201b-4f3d-8430-4dbf79064761
  :<<name>>-type_ALL-C: bug feature
  :<<name>>-type: bug
  :<<name>>-author: nil
  :<<name>>-priority: medium
  :END:

Each class has at least one super-class specified
by :iorg-super-C:, 'object' is at the top of the hierarchy and has
itself the pseudo-super-class 'root', that doesn't really exist.

The properties themselves can be classified into four categories:

1. Standard Org mode properties like :ID:

2. Properties with the :iorg-xyz: prefix that store general
information relevant for the iOrg framework

3. Properties with the :html-xyz: prefix that store information
relevant for the html-export by org-export.el with the derived
iorg-html backend.

4. Properties with the :<<name>>-xyz: prefix, where
<<name>> stands for the class name given by function
argument NAME, that store specific information relevant for
the class.

The properties with the `iorg-logic-class-property-suffix' are
like class variables in object-oriented programming. Their values
are constant for all instances/objects of the class, therefore
they are stored only once in file-local-variables that allow for
quick access.

The other properties are like instance variables in
object-oriented programming. Their values vary between
instances/objects of the class, therefore they are stored as
headline properties of every single object (which are entries in
Org files themselves). In the class file, an instance property's
value can be set either to nil or to a default value.

For each non-abstract class (classes tagged as :abstact: can't be
instantiated) there exists one Org file with all the
objects (instances) of that class."

  (interactive "sProject: \nsClass name: ")
  (cond ((not (and (non-empty-string-p project)
                   (assoc project iorg-projects-config)))
         (message "%s"
                  (concat "Project not registered in customizable "
                          "variable 'iorg-projects-config'")))
        ((not (non-empty-string-p name))
         (message "Not a valid class name."))
        (t
         (let* ((classname-nondir
                 (concat name "-class.org"))
                (classname
                 (expand-file-name
                  classname-nondir
                  (iorg-projects-get-project-info
                   project :classes)))
                (tags
                 (and tags-and-properties
                      (assoc :tags tags-and-properties)))
                (properties
                 (and tags-and-properties
                      (remove
                       (assoc :tags tags-and-properties)
                       tags-and-properties))))

           (with-current-buffer (find-file classname)
             (org-check-for-org-mode)
             (goto-char (point-min))
             ;; insert headline
             (insert
              (format
               ;; abstract class?
               (if (and tags (member "abstract" tags))
                   iorg-logic-abstract-class-headline-format-string
                 iorg-logic-class-headline-format-string)
               name))
             (newline)
             ;; insert top-level entry
             (if todo
                 (org-insert-todo-heading 1)
               (org-insert-heading))
             ;; (optionally) insert tags
             (insert name)               
             (and tags (org-set-tags-to (cdr tags)))
             ;; insert global ID property
             (org-id-get-create)
             ;; insert other properties
             (and properties
                  (mapc
                   (lambda (x)
                     (org-set-property
                      ;; make key symbol a string
                      ;; and get rid of leading ':'
                      (cadr (split-string
                             (format "%s" (car x))
                             ":"))
                      (cdr x)))
                   properties))
             (save-buffer)
             (show-all)))))) 


(defun iorg-logic-new-object (project class)
  "Instantiate a new object of CLASS in PROJECT. CLASS is the string that appears as text in the top-level headline of the class file, (e.g. 'task'). PROJECT is the string that appears as a key in 'iorg-projects-config', e.g. (e.g. 'bugpile').

In the context of iOrg, an object is a top-level entry in the Org
file for its class (<<class>>-obj.org), located in the projects
'objects' directory which is defined under the key :objects in
the customizable variable 'iorg-projects-config'.

Each object has a headline and optional todo's and tags. The
instance properties of the class are represented by
headline-properties of the object. Thus, two typical objects of
class <<classname>> might look similar to this:

# -*- variable-name:default-value-*-
* TODO <<classname>>                   :foo:
  :PROPERTIES:
  :ID:       7bcbaee7-2e1b-4f3d-8430-edbf79364761
  :iorg-x:  value1
  :html-edit:  link
  :<<classname>>-type: feature
  :<<classname>>-author: author1
  :<<classname>>-priority: high
  :END:
* DONE <<classname>>                   :bar:
  :PROPERTIES:
  :ID:       4bc3aee5-2ed21b-433d-8630-fdbf59364761
  :iorg-x:  value3
  :html-edit:  button
  :<<classname>>-type: bug
  :<<classname>>-author: author2
  :<<classname>>-priority: medium
  :END:

At the top of the Org file, the class properties of the class and
its super-classes are defined as file-local-variables. Then the
objects follow, each as an top-level entry.

The global :ID: property is set by Org mode and obligatory for
each object. The :<<classname>>-xyz: properties are read from the
objects class (its instance properties). All instance properties
from the super-classes of the class are included too, therefore
properties with other prefixes (like :iorg-xyz: or :html-xyz:)
might appear in an objects property drawer."

  (interactive "sProject: \nfClass: ")
  (cond ((not (and (non-empty-string-p project)
                   (assoc project iorg-projects-config)))
         (message "%s"
                  (concat "Project not registered in customizable "
                          "variable 'iorg-projects-config'")))
        (not (non-empty-string-p class)
         (message "Invalid class name"))
        (t
         (with-current-buffer
             (find-file
              (or
               (org-id-goto 
                (getkey (format
                         iorg-logic-object-regexp class)
                        org-id-locations))
               (expand-file-name
                (format iorg-logic-object-regexp class)
                (iorg-projects-get-project-info
                 project :objects))))

               

          (org-val-goto class)
          (org-check-for-org-mode)
          (save-restriction
            (widen)
          (while
              (progn
                ;; do the work
                (show-all) 
                (iorg-util-goto-first-entry)
                


                ;; test the condition
                (not
                 (string=
                  (car
                   (org-entry-get-multivalued-property
                    (point) iorg-super))
                  "root")))))))))

;; FIXME interactive - not-create y-or-n
;; FIXME eliminate redundancy
(defun iorg-logic-goto-objects-file (project class &optional not-create)
  "Selects a buffer visiting the Org file that contains all objects of CLASS in PROJECT. If NOT-CREATE is non nil, do not create a non existing class file."
  (interactive "sProject: \nsClass: ")
  (or
   (not
    (condition-case nil
        (org-val-goto class 'obj) ;successful search returns nil
      (error "%s" "Not found")))
   (if not-create
       (ignore-errors
         (find-file-existing
          (expand-file-name
           (concat class
                   iorg-logic-object-filename-tail)
           (iorg-projects-get-project-info project :objects))))
     (find-file
      (expand-file-name
       (concat class
               iorg-logic-object-filename-tail)
       (iorg-projects-get-project-info project :objects)))))) 
       
;; FIXME interactive: 'not-create' y-or-n
;; FIXME eliminate redundancy
(defun iorg-logic-goto-class-file (project class &optional not-create)
  "Selects a buffer visiting the Org file that contains the class definition of CLASS in PROJECT. If NOT-CREATE is non nil, do not create a non existing class file."
  (interactive "sProject: \nsClass: ")
  (or
   (not
    (condition-case nil
        (org-val-goto class) ;successful search returns nil
      (error "%s" "Not found")))
   (if not-create
       (ignore-errors
         (find-file-existing
          (expand-file-name
           (concat class
                   iorg-logic-class-filename-tail)
           (iorg-projects-get-project-info project :classes))))
     (find-file
      (expand-file-name
       (concat class
               iorg-logic-class-filename-tail)
       (iorg-projects-get-project-info project :classes))))))
       


(defun iorg-logic--build-object-from-class-hierarchy (project class)
  "Return a buffer-string that contains an iOrg object with all todo's, tags and properties having default values. 

The iOrg object is a top-level heading with a property drawer and
optionally todo's and tags. It is constructed by combining all
properties and tags from the object's CLASS in PROJECT and all
its super-classes."
  (with-temp-buffer
    (org-mode)
     (save-excursion
       (iorg-logic-goto-class-file
        project class 'not-create)
       (save-restriction
         (widen)
         (show-all)
         (iorg-util-goto-first-entry)
         (ignore-errors
         (org-copy-subtree))))
     (yank)
    (buffer-substring-no-properties
     (point-min) (point-max))))





    
;;; Public Functions (non-interactive)

(provide 'iorg-logic)

