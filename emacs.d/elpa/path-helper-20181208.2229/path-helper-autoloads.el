;;; path-helper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "path-helper" "path-helper.el" (0 0 0 0))
;;; Generated autoloads from path-helper.el

(autoload 'path-helper-setenv "path-helper" "\
Set the value of environment variable VARIABLE from its associated files.
Get a list of paths from '/etc/VARIABLEs' and '/etc/VARIABLEs.d/'.

The existing content of the environment variable VARIABLE is also
read, and paths not already found in the configuration files are
appended to the list.

As a special case, setting variable 'PATH' also sets `exec-path'.

\(fn VARIABLE)" t nil)

(autoload 'path-helper-setenv-all "path-helper" "\
Set the value of all environment variables in `path-helper-variables'.
Repeatedly call `path-helper-setenv' with each variable.

If `path-helper-skip-undefined-variables' is non-nil, environment
variables are only set to a new value when they were previously
set.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "path-helper" '("path-helper-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; path-helper-autoloads.el ends here
