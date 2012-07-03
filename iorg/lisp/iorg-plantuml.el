;;;; iorg-plantuml.el --- transforming PlantUML into Elisp code skeletons.

;;;; Requirements

;;;; Variables

;;;Consts

;;; Vars
(defvar iorg-plantuml-diagram-type-repexp
(concat "<\\(soa\\|csa\\|dcm\\)>")
  "Regexp used to identify plantuml diagramtypes from the
  plantuml 'titel' line in the Org-mode source block")

;;; Customs

;;;; Funtions

;;; Helper Functions


;;; Public Functions
(defun iorg-plantuml-transform-to-code (&optional file)
  "Transform all PlantUML source blocks in Org-file FILE into
Org-mode files (with entries) and Emacs Lisp files (with
functions and variables), following the transformation rules of
the iOrg framework."
  (with-current-buffer
      (if (and file (file-exists-p file))
          (find-file-existing file)
        (current-buffer))
    (case-fold-search t)
    (save-excursion
      (save-restriction
        (widen)
        (org-goto-line 1)
           (while (not (eobp))
             (if (not (looking-at org-babel-src-block-regexp))
                 (forward-line)
               (while (not (eolp))
                 (if (not (looking-at "plantuml"))
                     (forward-word)
                   (forward-line)
                   (while (not (looking-at "#+end_src")))))))))))


(provide 'iorg-plantuml)
