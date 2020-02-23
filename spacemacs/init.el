(setenv "DICPATH" (file-name-as-directory (expand-file-name "~/Library/Spelling/")))
(setenv "LANG" "en_US")
(setenv "DICTIONARY" "en_US")

(setq-default dotspacemacs-configuration-layers
  '(
    ;; Functionality
    auto-completion
    nlinum
    spell-checking
    syntax-checking

    ;; Editing Support
    org
    org-demi
    emoji
    typography

    ;; Languages
    asm
    (c-c++ :variables c-c++-enable-clang-support t)
    emacs-lisp
    html
    javascript
    lua
    nim
    python
    shell-scripts
    typescript
    windows-scripts


    ;; Applications
    ansible
    github
    nginx
    pandoc
    (shell :variables shell-default-term-shell "/brew/bin/fish")
    version-control

    ;; Data Files
    csv
    graphviz
    markdown
    yaml
    )
  )

(setq-default dotspacemacs-additional-packages
  '(
    ahg

    guide-key

    toml-mode

    xcode-project
    )
  )

(setq-default dotspacemacs-excluded-packages
  '(
    org-bullets ;; Disable the "fancy" org-mode bullets package
    )
  )

;; -------

(setq-default
  dotspacemacs-themes               '(moe-light)
  dotspacemacs-default-font         '("Fira Code" :size 16 :powerline-scale 1.2)
  dotspacemacs-editing-style        '(emacs)
  dotspacemacs-line-numbers         '(:enabled-for-modes prog-mode)
  dotspacemacs-maximized-at-startup t
  dotspacemacs-whitespace-cleanup   nil
)

(defun demi/user-config/defaults ()

  (defvar demi/directory-user
    (file-name-as-directory
     (getenv "HOME")))

  (defvar demi/xdg/directory-config
    (file-name-as-directory
     (expand-file-name ".config/" demi/directory-user)))

  (defvar demi/xdg/directory-cache
    (file-name-as-directory
     (expand-file-name ".cache/"  demi/directory-user)))

  ;; =========

  (defvar demi/org/directory-root
    (file-name-as-directory
     (expand-file-name "OrgFiles/" demi/directory-user)))

  (defvar demi/org/directory-assets
    (file-name-as-directory
     (expand-file-name "assets/" demi/org/directory-root)))

  (defvar demi/org/directory-data
    (file-name-as-directory
     (expand-file-name "data/" demi/org/directory-root)))

  ;; =========

  (defvar demi/org/file-diary
    (expand-file-name "project-diary.org" demi/org/directory-root))

  (defvar demi/org/file-books
    (expand-file-name "books.org" demi/org/directory-data))

  ;; =========

  (defvar demi/spacemacs/directory-root
    (file-name-as-directory
     (expand-file-name "spacemacs.d/" demi/xdg/directory-config)))

  (defvar demi/cache-emacs/directory-root
    (file-name-as-directory
     (expand-file-name "emacs/" demi/xdg/directory-cache)))

  (defvar demi/cache-emacs/directory-backups
    (file-name-as-directory
     (expand-file-name "backups/" demi/cache-emacs/directory-root)))

  (defvar demi/cache-emacs/directory-autosave
    (file-name-as-directory
     (expand-file-name "autosave/" demi/cache-emacs/directory-root)))

  ;; =========

  (defvar demi/org/file-diary
    (expand-file-name "project-diary.org" demi/org/directory-root))

  (defvar demi/org/file-books
    (expand-file-name "books.org" demi/org/directory-data))

  ;; =========

  (defvar demi/spacemacs/file-customize
    (expand-file-name "custom.el" demi/spacemacs/directory-root))

  ;; =========

  (defvar demi/cache-emacs/file-completions
    (expand-file-name "completions" demi/cache-emacs/directory-root))

  (defvar demi/cache-emacs/file-saved-positions
    (expand-file-name "saved-positions" demi/cache-emacs/directory-root))

  (defvar demi/cache-emacs/file-abbrev-defs
    (expand-file-name "abbrev-defs" demi/cache-emacs/directory-root))

  (defvar demi/cache-emacs/file-recentf
    (expand-file-name "recentf" demi/cache-emacs/directory-root))

  ;; =========

  (setq-default
   ;; Basics
   ring-bell-function    'ignore
   user-full-name        "Samantha Demi"
   user-mail-address     "hello@pewpewthespells.com"

   ;; Locations
   custom-file                demi/spacemacs/file-customize
   save-completions-file-name demi/cache-emacs/file-completions
   save-place-file            demi/cache-emacs/file-saved-positions
   abbrev-file-name           demi/cache-emacs/file-abbrev-defs
   recentf-save-file          demi/cache-emacs/file-recentf
   diary-file                 demi/org/file-diary
   org-books-file             demi/org/file-books

   ;; Displaying time and date
   display-time-24hr-format  t
   display-time-day-and-date t
   display-time-interval     15
   display-time-default-load-average nil

   ;; Sentence Rules
   sentence-end-double-space   nil
   sentence-end-without-period t

   ;; Backups
   create-lockfiles               t
   backup-by-copying              t
   vc-make-backup-files           t
   kept-new-versions              5
   kept-old-versions              5
   delete-old-versions            t
   vc-make-backup-files           t
   backup-directory-alist         '( ("." . "~/.cache/emacs/backups/") )
   auto-save-file-name-transforms '( (".*" "~/.cache/emacs/autosave/" t) )

   ;; Fonts
   scalable-fonts-allowed t

   ;; Indentation
   tab-width 2
   standard-indent 2
   fish-indent-offset 2
   indent-tabs-mode nil

   )

 ; (rainbow-delimiters-mode nil)
  (cua-mode t)
  (ido-mode 'both)
  (tooltip-mode t)
  (global-visual-line-mode t)

  )

(defun demi/user-config/theme ()
  (moe-theme-set-color 'magenta)
  (moe-light)
  (setq powerline-default-separator 'curve)
  )

(defun demi/user-config/interface ()
  (spacemacs/toggle-fullscreen-frame-off)
  (spacemacs/toggle-menu-bar-on)
  (spacemacs/toggle-tool-bar-on)
  (spacemacs/toggle-fringe-on)
  (spacemacs/toggle-display-time-on)

  (spacemacs/toggle-auto-fill-mode-on)
  (spacemacs/toggle-visual-line-navigation-on)
  (spacemacs/toggle-truncate-lines-off)

  (spacemacs/toggle-mode-line-version-control-on)
  (spacemacs/toggle-mode-line-battery-on)
  (spacemacs/toggle-mode-line-minor-modes-off)
  (spacemacs/toggle-display-time-on)
  (spacemacs/toggle-centered-point-globally-off)

  (spacemacs/toggle-smartparens-globally-off)
  (spacemacs/toggle-highlight-current-line-globally-off)

  (spaceline-toggle-evil-state-off)
  (spaceline-toggle-line-column-on)
  (spaceline-toggle-selection-info-on)
  (spaceline-toggle-org-clock-on)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-hud-off)

  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-flycheck-warning-on)
  (spaceline-toggle-flycheck-error-on)

  )

(defun demi/user-config/configure/org ()
  ;; Add custom keywords
  (setq org-todo-keywords '(
    ;; Template: (sequence "ACTIONABLE_STATE(abbrev)" "|" "COMPLETION_STATE(abbrev)")

    ;; General Tasks TODO Keywords
    (sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "FOLLOWUP(f)" "|" "DONE(d)" "CANCELED(c)" )

    ;; Meeting/Event TODO Keywords
    (sequence                                    "TENTATIVE(m)" "|" "CONFIRMED(y)" "DECLINED(n)" "CANCELED(c)" )

    )
  )

  (setq org-todo-keyword-faces '(
    ;; Template: ("KEYWORD" . face-definition)
    ;; Face Definition Template: (:foreground "color" :weight value :background "color" :box border-geometry)
    ;; Border Geometry Template: (:line-width integer)

    ;; General Tasks TODO Keyword Faces
    ("TODO"     . org-todo)
    ("STARTED"  . (:foreground "orange"  :weight bold :background "yellow" :box (:line-width 1)))
    ("WAITING"  . (:foreground "blue"    :weight bold :background "cyan"   :box (:line-width 1)))
    ("FOLLOWUP" . (:foreground "red"     :weight bold :background "orange" :box (:line-width 1)))
    ("DONE"     . org-done)
    ("CANCELED" . (:foreground "grey"    :weight bold :background "white"  :box (:line-width 1)))

    ;; Meeting/Event TODO Keyword Faces
    ("TENTATIVE" . (:foreground "orange" :weight bold :background "yellow" :box (:line-width 1)))
    ("CONFIRMED" . org-done)
    ("DECLINED"  . (:foreground "black"  :weight bold :background "grey"   :box (:line-width 1)))
    ("CANCELED"  . (:foreground "grey"   :weight bold :background "white"  :box (:line-width 1)))

    ;; Appointment TODO Keyword Faces
    ("ASAP"      . org-todo)
    ("REQUESTED" . (:foreground "orange" :weight bold :background "yellow" :box (:line-width 1)))
    ("BOOKED"    . org-done)
    ("CANCELED"  . (:foreground "grey"   :weight bold :background "white"  :box (:line-width 1)))

    )
  )

  ;; Insert date+time of marking a TODO item to a completed state.
  (setq org-log-done 'time)

  ;; If a TODO item is altered from a completed_state to an actionable_state, preserve the "CLOSED:" entry it has
  (setq org-closed-keep-when-no-todo t)

  ;; Set path to the default directory that Org-mode files should be found in.
  (setq org-directory demi/org/directory-root)

  ;; Set the search pattern for generating Org-mode's Agenda.
  (setq org-agenda-files (directory-files org-directory t "^(project\-)?.*\.org$"))

  )

(defun demi/user-config/configure/ispell ()
  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,en_GB,Medical,hyphen_en_US,hyph_en_GB")
  (setq ispell-dictionary "en_US") ;,en_GB,Medical,hyphen_en_US,hyph_en_GB")
  )

(defun demi/save-and-commit ()
  (when (eq major-mode 'org-mode) (message "do a thing on save") )
  )

(defun demi/disable-spell-check ()
  (when (eq major-mode 'prog-mode) (flyspell-mode-off) )
  )

(defun demi/user-config/hooks ()
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode)
  (add-hook    'after-save-hook      'demi/save-and-commit)
  (add-hook    'prog-mode-hook       'demi/disable-spell-check)

  (with-eval-after-load 'org    'demi/user-config/configure/org)
  (with-eval-after-load 'ispell 'demi/user-config/configure/ispell)
  )


(defun dotspacemacs/user-config ()
  (demi/user-config/defaults)
  (demi/user-config/interface)
  (demi/user-config/theme)
  (demi/user-config/hooks)
  )
