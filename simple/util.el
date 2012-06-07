;;; util.el --- collection of utility functions

(defun assoc-proc (proc list)
  "Like `assoc' but return the first pair whose `car' matches PROC."
  (dolist (pair list)
    (when (funcall proc (car pair))
      (return pair))))

(defun assoc-re (re list &optional count)
  "Return the first pair in LIST whose `car' matches RE. If COUNT is non nil, return instead the text matched by `string-match'. If COUNT is zero, return the entire matched text, otherwise just the portion corresponding to the COUNTth parenthetical subexpression."
  (let* ((in-string)
         (assoc
          (assoc-proc
           (lambda (it)
             (let ((str (format "%s" it)))
               (and
                (string-match re str)
                (setq in-string str))))
               list)))
         (if (and count (integerp count))
             (match-string count in-string)
           assoc)))

(defmacro while-visiting-file (file &rest body)
  "Execute BODY in a temporary buffer visiting FILE."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file ,file)
     ,@body))
(def-edebug-spec while-visiting-file (form body))

(provide 'util)
