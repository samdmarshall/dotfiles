(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             )
(package-initialize)

; ========================================================== ;

(add-to-list 'load-path "~/.emacs.d/emacs-nav")

(require 'nav)
(require 'redo+)
(require 'indent-guide)
(require 'mouse)
(require 'hlinum)

; ========================================================== ;


(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))
    (set-face-background 'linum "light grey" (selected-frame))
    ))
(add-hook 'window-setup-hook 'on-after-init)


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

;; add support for scrolling with the mouse
(global-set-key [(mouse-4)] 'previous-line)
(global-set-key [(mouse-5)] 'next-line)

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


(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)

(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-d") 'kill-whole-line)

(global-set-key (kbd "C-s") 'save-buffer) ;; Save
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs) ;; Save & Quit
(global-set-key (kbd "C-w") 'kill-buffer) ;; Quit
(global-set-key (kbd "C-f") 'isearch-forward) 
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-y") 'redo)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-o") 'find-file)

(global-set-key (kbd "C-c") 'kill-ring-save) ;; Copy
(global-set-key (kbd "C-x") 'kill) ;; Cut
(global-set-key (kbd "C-v") 'yank) ;; Paste

(global-set-key (kbd "<f8>") 'nav-toggle)


;;
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


;;(set-default 'cursor-type 'box)
