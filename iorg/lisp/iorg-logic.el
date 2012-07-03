;;; iorg-logic --- generic logic for the iOrg framework. 

(defun iorg-logic-new-object (class)
  "Instantiate a new object of CLASS

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

  (interactive "fClass: ")
  


(provide 'iorg-logic)
