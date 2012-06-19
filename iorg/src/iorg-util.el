;;; iorg-util.el --- utility functions

;;; Code:
(defmacro while-visiting-file (file &rest body)
  "Execute BODY in a temporary buffer visiting FILE."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file ,file)
     ,@body))
(def-edebug-spec while-visiting-file (form body))

(defun file-contents (path)
  (while-visiting-file path (buffer-string)))

(defvar iorg-base
  (file-name-directory (or load-file-name (buffer-file-name))))

(provide 'iorg-util)
;;; iorg-util.el ends here
