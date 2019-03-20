;; Setup Emacs Package Sources, this is vital so that `use-package` can install them if they aren't already installed locally
(load "package")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Make use of the super-convenient `use-package` to fetch and install packages as part of start-up
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Compose the path "~/.emacs.d/components/" then get all ".el" files within that directory
(setq absolute-user-init-directory (file-name-directory user-init-file))
(setq components-path (expand-file-name "components" absolute-user-init-directory))
(dolist (path (directory-files components-path t "^.*\.el$"))
  (message "%S" path)
  (load path))

;;
(setq user-emacs-customize-file (expand-file-name "customize.el" absolute-user-init-directory))
(load user-emacs-customize-file)

