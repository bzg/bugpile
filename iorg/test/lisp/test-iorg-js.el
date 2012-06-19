;;; test-iorg-js.el -- tests for org-e-iorg-js
(require 'ert)
(require 'org-test)
(require 'iorg-js)
(require 'iorg-server)

(defvar test-iorg-js-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defvar test-iorg-js-example-dir
  (expand-file-name "examples" (expand-file-name ".." test-iorg-js-dir)))

(defvar test-iorg-js-simple-file
  (expand-file-name "simple.org" test-iorg-js-example-dir))

(ert-deftest iorg-js-simple-export ()
  (flet ((has (it)
              (goto-char (point-min))
              (should (re-search-forward (regexp-quote it) nil t))))
    (let ((html-file (org-test-with-temp-text-in-file
                         (file-contents test-iorg-js-simple-file)
                       (iorg-js-export-to-html))))
      (while-visiting-file html-file
        ;; should include the ihtml css header
        (has ".editable")
        ;; should include the ihtml javascript header
        (has "$(document).ready(function(){ set_clickable(); });")
        ;; the paragraph should be editable
        (has "<div class=\"edit_in_place\"><p>")
        ;; the plain list should be editable
        (has "<div class=\"edit_in_place\"><ul>")
        ;; the elements of the plain list should not be editable
        (has "<li>first")))))

(ert-deftest iorg-js-export-file ()
  (let ((html-file (iorg-js-export-file test-iorg-js-simple-file)))
    (should (file-exists-p html-file))))

;;; test-iorg-js.el ends here
