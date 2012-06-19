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

(defun non-empty-string-p (str)
  "Return t if function argument STR is a string of length > 0, nil otherwise."
 (if (and (stringp str) (> (length str) 0))
     str
   nil))

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
  (expand-file-name
   ".." (file-name-directory (or load-file-name (buffer-file-name)))))

(defvar iorg-lisp
  (expand-file-name "lisp" iorg-base))

(defvar iorg-src
  (expand-file-name "src" iorg-base))

(provide 'iorg-util)
