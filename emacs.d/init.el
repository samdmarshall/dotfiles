(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             )
(package-initialize)

; ========================================================== ;

(add-to-list 'load-path "~/.emacs.d/emacs-nav")
(add-to-list 'load-path "/home/linuxbrew/.linuxbrew/share/emacs/site-lisp/pass/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/moe-theme")
(add-to-list 'load-path "~/.emacs.d/moe-theme")

(require 'moe-theme)
(require 'nav)
(require 'redo+)
(require 'indent-guide)
(require 'mouse)
(require 'hlinum)
(require 'ido)

; ========================================================== ;

;; Tab Bar
(ido-mode t)

;; Theme
(setq moe-theme-highlight-buffer-id t)
(moe-theme-set-color 'black)
(load-theme 'moe-light t)
(moe-light)

;; # Indent Guides
(indent-guide-global-mode)

;; # Tab-complete for Nim
(setq nim-nimsuggest-path "/home/demi/.nimble/bin/nimsuggest")
(add-hook 'nim-mode-hook 'nimsuggest-mode)

;; # Tab settings
(setq-default tab-width 2)
(setq tab-width 2)

;; # Mouse support
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; now disable bells for most things because this is annoying af
(setq ring-bell-function 'ignore)

;; enabling line numbers for files
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

(hlinum-activate)
(defface linum-highlight-face
  '((t (:bold :inherit default :foreground "black")))
  :group 'linum)

;; Loading the quick file navigator
(nav-disable-overeager-window-splitting)

;; commands for nav-mode
(defun nav-mode-hl-hook ()
  (local-set-key (kbd "<right>") 'nav-open-file-under-cursor)
  (local-set-key (kbd "<left>")  'nav-go-up-one-dir))
(add-hook 'nav-mode-hook 'nav-mode-hl-hook)

(defface nav-hl-line
  '((t :background "gray")) ; change to what suits best your theme
  "Custom face for highlighting the current line in nav mode."
  :version "22.1"
  :group 'hl-line)
;; This allows global-hl-line be disabled for certain buffers (nav in our case)
(make-variable-buffer-local 'global-hl-line-mode)

(defun nav-mode-hl-hook ()
  (global-hl-line-mode)
  (set (make-local-variable 'hl-line-face) 'nav-hl-line)
  (hl-line-mode t)
  (local-set-key (kbd "<right>") 'nav-open-file-under-cursor)
  (local-set-key (kbd "<left>")  'nav-go-up-one-dir))
(add-hook 'nav-mode-hook 'nav-mode-hl-hook)

;; configure auto-save and backup files:
(setq backup-directory-alist `(("." . "~/.cache/emacs/")))
(setq backup-by-copying t)

;; Mode Line Settings
(load "~/.emacs.d/mode-line.el")

;; Import Keybindings
(load "~/.emacs.d/keymap.el")

;; Import Ligature Support
(load "~/.emacs.d/fira-code.el")

;; Import Customization Settings
(load "~/.emacs.d/customizations.el")
