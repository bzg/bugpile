(require 'ert)
(require 'simple)
(require 'util)

(ert-deftest iorg-util-assoc-proc ()
  (should (tree-equal
           (assoc-proc #'evenp '((1 . :foo) (2 . :baz) (4 . :qux)))
           '(2 . :baz))))


;; TODO: update test to new implementation
(ert-deftest iorg-util-assoc-re ()
  (should (tree-equal
           (assoc-re "outline-[[:digit:]]-[[:digit:]]"
                     '((foo . bar)
                       (outline . baz)
                       (outline-2-4 . qux)
                       (qux . quxx)))
           '(outline-2-4 . qux))))

(ert-deftest iorg-org-to-html-creates-output-file-if-doesnt-exist ()
  (let ((input-file "simple.org")
        (output-file "simple.html"))
    ;; ensure the output file is not already present
    (when (file-exists-p output-file) (delete-file output-file))
    (should (not (file-exists-p output-file)))
    ;; call iorg--org-to-html to generate the output file
    (should (string= (expand-file-name output-file)
                     (iorg--org-to-html input-file)))
    ;; ensure that the output file exists
    (should (file-exists-p output-file))
    ;; ensure that the output file is not empty
    (should (< 0 (while-visiting-file output-file
                   (- (point-max) (point-min)))))))
