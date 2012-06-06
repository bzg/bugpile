;;; util.el --- collection of utility functions

(defun assoc-proc (proc list)
  "Like `assoc' but return the first pair whose `car' matches PROC."
  (dolist (pair list)
    (when (funcall proc (car pair))
      (return pair))))

(defun assoc-re (re list)
  "Return the first pair in LIST whose `car' matches RE."
  (assoc-proc (lambda (it) (string-match re (format "%s" it))) list))

(defmacro while-visiting-file (file &rest body)
  "Execute BODY in a temporary buffer visiting FILE."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file ,file)
     ,@body))
(def-edebug-spec while-visiting-file (form body))

(provide 'util)
