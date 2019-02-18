;;; how-many-lines-in-project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "how-many-lines-in-project" "how-many-lines-in-project.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from how-many-lines-in-project.el

(autoload 'hm-lines-in-project "how-many-lines-in-project" "\
Calculate how many lines are there in your project.

\(fn)" t nil)

(defalias 'how-many-lines-in-project 'hm-lines-in-project)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "how-many-lines-in-project" '("hm-lines-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; how-many-lines-in-project-autoloads.el ends here
