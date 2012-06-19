;;; org-test-e-iorg-js -- tests for org-e-iorg-js
(require 'ert)
(require 'org-test)
(require 'org-e-iorg-js)
(require 'iorg-server)

(defvar test-e-iorg-js-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defvar test-e-iorg-js-example-dir
  (expand-file-name "examples" (expand-file-name ".." test-e-iorg-js-dir)))

(defvar test-e-iorg-js-simple-file
  (expand-file-name "simple.org" test-e-iorg-js-example-dir))

(ert-deftest iorg-js-simple-export ()
  (flet ((has (it)
              (goto-char (point-min))
              (should (re-search-forward (regexp-quote it) nil t))))
    (let ((html-file (org-test-with-temp-text-in-file
                         (file-contents test-e-iorg-js-simple-file)
                       (org-e-iorg-js-export-to-html))))
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
  (let ((html-file (org-e-iorg-js-export-file test-e-iorg-js-simple-file)))
    (should (file-exists-p html-file))))

;;; org-test-e-ihtml ends here
