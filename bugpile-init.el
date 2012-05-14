;;; <<project>>-init.el --- Where all the magic begins
;;
;; Part of the iOrg Webframework
;;
;; This is the first thing to get loaded.
;;

;; remember this directory
(setq (concat iorg-project-name "-dir")
      (file-name-directory (or load-file-name (buffer-file-name))))

;; load up the starter kit
(org-babel-load-file (expand-file-name "starter-kit.org" project-dir))

;;; <<project>>-init.el ends here
