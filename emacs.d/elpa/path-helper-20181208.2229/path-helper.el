;;; path-helper.el --- Set PATH environment variables from config files  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Arnaud Rouanet

;; Author: Arnaud Rouanet <arnaud@rouanet.org>
;; Created: 29 Oct 2018
;; Version: 1.1
;; Package-Version: 20181208.2229
;; Keywords: tools, unix
;; Homepage: https://github.com/arouanet/path-helper
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file replicates the behavior of the path_helper(8) utility
;; shipped with macOS, used to augment the PATH and MANPATH environment
;; variables with path elements read respectively from /etc/paths and
;; /etc/paths.d/ for PATH, and from /etc/manpaths and /etc/manpaths.d/
;; for MANPATH.

;; This is needed because macOS GUI applications are launched with
;; a limited default environment which is not set from the user's login
;; shell.  While it is possible to configure launchd to use a different
;; PATH variable (using "launchctl config user path"), another solution
;; is to set the PATH after the fact during Emacs initialization, which
;; is what this file is doing.

;; This is similar to what the exec-path-from-shell package does, but
;; because path-helper does not need to spawn a shell to read the
;; environment, it is much faster.

;; An obvious downside of this approach is that if the PATH is manually
;; set elsewhere, such as in the user .profile file, it will not be
;; visible to path-helper.  But properly configured macOS packages such
;; as MacTeX, which contribute to the PATH by adding a file in
;; /etc/paths.d/, will work as expected.

;; To set all environment variables listed in `path-helper-variables',
;; add the following to your init file:

;;     (when (memq window-system '(ns mac))
;;       (path-helper-setenv-all))

;; Or, if using use-package:

;;     (use-package path-helper
;;       :if (memq window-system '(mac ns))
;;       :ensure t
;;       :config
;;       (path-helper-setenv-all))

;; If `path-helper-skip-undefined-variables' is non-nil (default),
;; environment variables are only set to a new value when they were
;; previously set.  This is because MANPATH is generally unset by
;; default, and it is preferable to leave it unset and let `man' use its
;; more sophisticated method of finding manual page files.

;; You can also call `path-helper-setenv' directly to set a single
;; environment variable, e.g.:

;;     (path-helper-setenv "PATH")

;;; Code:

(defgroup path-helper nil
  "Set PATH environment variables from configuration files."
  :prefix "path-helper-"
  :group 'unix)

(defcustom path-helper-variables '("PATH" "MANPATH")
  "The list of variables potentially set by `path-helper-setenv-all'."
  :type '(repeat (string :tag "Environment variable"))
  :group 'path-helper)

(defcustom path-helper-skip-undefined-variables t
  "If non-nil, only existing variables are set by `path-helper-setenv-all'."
  :type 'boolean
  :group 'path-helper)

(defun path-helper--file-lines (filename)
  "Return a list of non-empty lines from the file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(defun path-helper--paths-from-files (filename)
  "Return a list of paths from the file FILENAME and directory FILENAME.d.
If it exists, the content of the file FILENAME is returned first,
followed by the content of each file in the FILENAME.d directory.

The files should contain one path element per line.  Empty lines
are ignored."
  (append
   (when (file-exists-p filename)
     (path-helper--file-lines filename))
   (let ((directory (concat filename ".d")))
     (when (file-directory-p directory)
       (mapcan
        'path-helper--file-lines
        (reverse (directory-files directory t
                                  directory-files-no-dot-files-regexp t)))))))

;;;###autoload
(defun path-helper-setenv (variable)
  "Set the value of environment variable VARIABLE from its associated files.
Get a list of paths from '/etc/VARIABLEs' and '/etc/VARIABLEs.d/'.

The existing content of the environment variable VARIABLE is also
read, and paths not already found in the configuration files are
appended to the list.

As a special case, setting variable 'PATH' also sets `exec-path'."
  (interactive
   (list
    (completing-read "Set environment variable: " path-helper-variables)))
  (let ((filename (concat "/etc/" (downcase variable) "s"))
        (existing-value (getenv variable)))
    (let ((paths (delete-dups
                  (append (path-helper--paths-from-files filename)
                          (when existing-value
                            (split-string existing-value path-separator))))))
      (setenv variable (mapconcat #'identity paths path-separator))
      (when (string-equal "PATH" variable)
        (setq exec-path (append (mapcar #'file-name-as-directory paths)
                                (list exec-directory)))))))

;;;###autoload
(defun path-helper-setenv-all ()
  "Set the value of all environment variables in `path-helper-variables'.
Repeatedly call `path-helper-setenv' with each variable.

If `path-helper-skip-undefined-variables' is non-nil, environment
variables are only set to a new value when they were previously
set."
  (interactive)
  (dolist (variable path-helper-variables)
    (when (or (not path-helper-skip-undefined-variables)
              (getenv variable))
      (path-helper-setenv variable))))

(provide 'path-helper)

;;; path-helper.el ends here
