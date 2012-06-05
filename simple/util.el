;;; util.el --- collection of utility functions

(defmacro while-visiting-file (file &rest body)
  "Execute BODY in a temporary buffer visiting FILE."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file ,file)
     ,@body))
(def-edebug-spec while-visiting-file (form body))

(provide 'util)
