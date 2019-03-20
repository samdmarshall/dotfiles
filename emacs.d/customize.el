(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(setq tab-width 2)
(setq indent-tabs-mode nil)

(setq ring-bell-function 'ignore)

(setq make-backup-files nil)


(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(require 'mouse)
(xterm-mouse-mode t)

(require 'ido)
(ido-mode t)

(global-visual-line-mode t)

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
