(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(package-initialize)

(setq config-org-file (expand-file-name "config.org" user-emacs-directory))
(setq config-el-file  (expand-file-name "config.el"  user-emacs-directory))

(defun last-modified-date (path)
  (nth 5 (file-attributes path)))

(setq org-file-modified (last-modified-date config-org-file))
(setq el-file-modified  (last-modified-date config-el-file))

(when (time-less-p org-file-modified el-file-modified)
  (org-babel-load-file config-org-gile))
(load-file config-el-file)

(setq gc-cons-threshold (* 2 1000 1000))

