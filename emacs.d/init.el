(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             )
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/emacs-colors-solarized")

(require 'color-theme-solarized)
(color-theme-solarized-light)

(require 'tabbar)
(tabbar-mode t)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))
    (set-face-background 'linum "light grey" (selected-frame))
    ))
(add-hook 'window-setup-hook 'on-after-init)

;; enabling indent guide to show where indents are
(require 'indent-guide)
(indent-guide-global-mode)

(require 'ergoemacs-mode)

(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

(define-key global-map (kbd "M-O") nil)

;; re-enable the arrow keys for navigation
(global-set-key [(left)]  'left-char)
(global-set-key [(right)] 'right-char)
(global-set-key [(up)]    'previous-line)
(global-set-key [(down)]  'next-line)

(global-set-key (kbd "C-K") 'kill-line)
(global-set-key (kbd "C-D") 'kill-whole-line)
(global-set-key (kbd "C-Q") 'save-buffers-kill-terminal)

;; enabling support for nimsuggest completions
(setq nim-nimsuggest-path "/usr/local/bin/nimsuggest")
(add-hook 'nim-mode-hook 'nimsuggest-mode)

;; use tabs for indent, but tabbing again should trigger completions
(setq tab-always-indent 'complete)

;; enable mouse support
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; add support for scrolling with the mouse
(global-set-key [(mouse-4)] 'previous-line)
(global-set-key [(mouse-5)] 'next-line)

;; now disable bells for most things because this is annoying af
(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; enabling line numbers for files
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

(require 'hlinum)
(hlinum-activate)
(defface linum-highlight-face
  '((t (:bold :inherit default :foreground "black")))
  :group 'linum)

;; Loading the quick file navigator
(add-to-list 'load-path "~/.emacs.d/emacs-nav")
(require 'nav)
(nav-disable-overeager-window-splitting)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "c5798c93000c3ababfe11a0d7cd2c82ca2167241ff6b99136506aade4ae8a03e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (tabbar-ruler color-theme hlinum vlf vdiff spinner solarized-theme rainbow-mode on-screen nlinum nim-mode indent-guide fish-mode ergoemacs-mode)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c37300")
     (60 . "#b97d00")
     (80 . "#b58900")
     (100 . "#a18700")
     (120 . "#9b8700")
     (140 . "#948700")
     (160 . "#8d8700")
     (180 . "#859900")
     (200 . "#5a942c")
     (220 . "#439b43")
     (240 . "#2da159")
     (260 . "#16a870")
     (280 . "#2aa198")
     (300 . "#009fa7")
     (320 . "#0097b7")
     (340 . "#008fc7")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
