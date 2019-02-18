;;; nm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nm" "nm.el" (0 0 0 0))
;;; Generated autoloads from nm.el

(autoload 'nm "nm" "\
Switch to *nm* buffer and load files.

\(fn)" t nil)

(autoload 'nm-jotmuch "nm" "\
Switch to *nm* buffer and run jotmuch.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nm" '("nm-")))

;;;***

;;;### (autoloads nil "nm-company" "nm-company.el" (0 0 0 0))
;;; Generated autoloads from nm-company.el

(autoload 'nm-company "nm-company" "\
`company-mode' completion back-end for `nevermore (nm)'.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nm-company" '("nm-company-last-prefix")))

;;;***

;;;### (autoloads nil "nm-dateparse" "nm-dateparse.el" (0 0 0 0))
;;; Generated autoloads from nm-dateparse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nm-dateparse" '("nm-" "set-time-of-day" "SECONDS-IN-DAY")))

;;;***

;;;### (autoloads nil nil ("nm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nm-autoloads.el ends here
