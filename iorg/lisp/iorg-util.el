;;;; iorg-util.el --- collection of utility functions for iOrg

;;;; Requirements

(eval-when-compile
  (require 'cl)
  )
;; FIXME necesary for getkey functions?
;; (require 'cl)  

;;;; Variables
;;; Consts
;;; Vars

;; (defvar iorg-base
;;   (expand-file-name
;;    ".." (file-name-directory
;;          (or load-file-name (buffer-file-name)))))

;; (defvar iorg-lisp
;;   (expand-file-name "lisp" iorg-base))

;; (defvar iorg-src
;;   (expand-file-name "src" iorg-base))

;;; Customs

;;;; Functions
;;; Function Declarations
;;; Helper Functions 
;;; Public Functions (interactive)

;;; Public Functions (non-interactive - generic emacs functions)

;; 'getkey(s)' functions courtesy of Pascal Bourguignon
(defun* getkey (val table &key (test (function string-match)) (default nil))
   ;; (hash-table-test table)  is for keys, not for values…
   (maphash (lambda (k v) 
              (when (funcall test val v) (return-from getkey k)))
            table)
   default)

;; Usage example: 
;; (let ((h (make-hash-table)))
;;   (setf (gethash :one h) "un"
;;         (gethash :two h) "deux"
;;         (gethash :a   h) "un")

;;   (getkey "un" h :test (function string=)))
;; --> :one ; or :a, who knows?


;; Bastiens approach: 
;; (let (key-found)
;;   (maphash (lambda (key val)
;; 	     (when (equal val "my_value")
;; 	       (setq key-found key)))
;; 	   hashtest)
;;   key-found)


(defun* getkeys (val table &key (test (function string-match)))
   ;; (hash-table-test table)  is for keys, not for values…
   (let ((keys '()))
     (maphash (lambda (k v) 
                 (when (funcall test val v) (push k keys)))
              table)
     keys))

;; Usage example:
;; (let ((h (make-hash-table)))
;;   (setf (gethash :one h) "un"
;;         (gethash :two h) "deux"
;;         (gethash :a   h) "un")
;;   (getkeys "un" h :test (function string=)))
;; --> (:a :one)


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

;;; Public Functions (non-interactive - iorg utility functions)

(defun iorg-util-goto-first-entry (&optional file)
  "Move point to the beginning of line of the first entry in the
current buffer or FILE."
  (with-current-buffer
      (if (and file (file-exists-p file))
          (find-file-existing file)
        (current-buffer))
    (org-goto-line 1)
    (or (looking-at org-outline-regexp)
        (re-search-forward org-outline-regexp-bol nil t))
    (beginning-of-line)))

(defun iorg-util-goto-last-entry (&optional file)
  "Move point to the beginning of line of the last entry in the
current buffer or FILE."
  (with-current-buffer
      (if (and file (file-exists-p file))
          (find-file-existing file)
        (current-buffer))
    (org-goto-line
     (line-number-at-pos (point-max))
     (or (looking-at org-outline-regexp)
         (re-search-backward org-outline-regexp-bol nil t))
     (beginning-of-line))))


(defmacro iorg-util-in-file (file &rest body)
  "Execute BODY in a buffer visiting FILE.
    If no buffer exists visiting FILE then open FILE in a new buffer."
  `(save-window-excursion
     (condition-case nil
         (with-temp-buffer (find-file ,file) ,@body)
       (error (message "iorg: there has been an error")))))

;; (iorg-util-in-file "/tmp/something.org" (message "in %s" (current-buffer)))

(defun iorg-util-pwd ()
  "Return the (normalized) directory part of the function `pwd'."
  (expand-file-name
   (file-name-as-directory
    (cadr (split-string (pwd) " ")))))

(defun iorg-util-replace-filename-prefix (old-prefix new-prefix &optional dir)
  "Replace OLD-PREFIX with NEW-PREFIX in filename of
all (non-directory) files in present working directory or DIR."
  (let ((proj
         (if dir
             (iorg-util-normalize-existing-dir-name dir)
           (iorg-util-pwd))))
    (condition-case err
        (mapc
         (lambda (x)
           (and
            (string-match
             (concat "\\(^\\)\\(" old-prefix "\\)\\(.+\\)\\($\\)") x)
            (not (file-directory-p (concat proj x)))
            (let* ((first-part (match-string 2 x))
                   (last-part (match-string 3 x)))
              (and first-part last-part
                   (rename-file
                    (concat proj first-part last-part)
                    (concat proj new-prefix last-part) t)))))
         (directory-files proj))
    ;; error handler
    (error 
     (princ
      (format
       "Error replacing the filename-prefix: %s" err))
     nil))))

(defun iorg-util-normalize-existing-dir-name (dir)
  "Return name of existing DIR in canonical form"
  (if (file-directory-p dir)
      (expand-file-name (file-name-as-directory dir))
    (message "Not a directory name")))

  
(defun iorg-util-normalize-new-dir-name (name)
  "Return NAME for a new directory in canonical form"
  (and (non-empty-string-p name)
       (expand-file-name (file-name-as-directory name))))


(defun iorg-util-filter-multival-property (prop reg)
  "Returns a list of strings with all elements of MULTIVAL-PROP
that match REGEXP."
  (remove nil 
          (mapcar
           (lambda (x)
             (and (string-match-p reg x) x))
           prop)))


(provide 'iorg-util)
