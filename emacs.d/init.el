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

;; start server-mode
(server-start)

;; over-write selection
(delete-selection-mode)

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

;;(set-default 'cursor-type 'box)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (moe-light)))
 '(custom-safe-themes
	 (quote
		("13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" default)))
 '(font-use-system-font t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 (quote
		(w3m wc-mode guess-language helpful how-many-lines-in-project link nasm-mode nav nm password-store password-store-otp path-helper yaml-mode tss toml-mode toml tide redo+ pass npm-mode nlinum nim-mode markdown-mode indent-guide homebrew-mode hlinum fzf fish-mode elscreen electric-case)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 121 :width normal)))))
