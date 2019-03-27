(setenv "DICPATH" (file-name-as-directory (expand-file-name "~/Library/Spelling/")))
(setenv "LANG" "en_US")
(setenv "DICTIONARY" "en_US")

(setq-default dotspacemacs-configuration-layers
  '(
    ;; Functionality
    (auto-completion)
    (nlinum)
    (spell-checking
     :variables
     enable-flyspell-auto-completion t
     )
    (syntax-checking)

    ;; Editing Support
    (org)
    (emoji)
    (typography)

    ;; Languages
    (asm)
    (c-c++
     :variables
     c-c++-enable-clang-support t
     )
    (emacs-lisp)
    (html)
    (javascript)
    (lua)
    (nim)
    (python)
    (shell-scripts)
    (typescript)
    (windows-scripts)


    ;; Applications
    (ansible)
    (github)
    (nginx)
    (pandoc)
    (shell
     :variables
     shell-default-term-shell "/home/linuxbrew/.linuxbrew/bin/fish"
     )
    (version-control)

    ;; Data Files
    (csv)
    (graphviz)
    (markdown)
    (yaml)
    )
  )

(setq-default dotspacemacs-additional-packages
  '(
    ;; Org Mode Additions
    (calfw :variables calendar-week-start-day 1)
    (calfw-cal)
    (calfw-ical)
    (calfw-org)
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
  (defvar user-home-directory  (getenv "HOME"))
  (defvar xdg-config-directory (getenv "XDG_CONFIG_HOME"))
  (defvar xdg-cache-directory  (getenv "XDG_CACHE_HOME"))

  (defvar demi/org-project-directory
    (file-name-as-directory (expand-file-name "org/" user-home-directory)))
  (defvar demi/spacemacs-directory
    (file-name-as-directory (expand-file-name "spacemacs.d/" xdg-config-directory)))
  (defvar demi/emacs-cache-directory
    (file-name-as-directory (expand-file-name "emacs/" xdg-cache-directory)))
  (defvar demi/emacs-backups-directory
    (file-name-as-directory (expand-file-name "backups/" demi/emacs-cache-directory)))
  (defvar demi/emacs-autosave-directory
    (file-name-as-directory (expand-file-name "autosave/" demi/emacs-cache-directory)))

  (defvar demi/diary-file
    (expand-file-name "project-diary.org" demi/org-project-directory))
  (defvar demi/customize-file
    (expand-file-name "custom.el" demi/spacemacs-directory))
  (defvar demi/save-completions-file-name
    (expand-file-name "completions" demi/emacs-cache-directory))
  (defvar demi/save-place-file
    (expand-file-name "saved-positions" demi/emacs-cache-directory))
  (defvar demi/abbrev-file-name
    (expand-file-name "abbrev-defs" demi/emacs-cache-directory))
  (defvar demi/recentf-save-file
    (expand-file-name "recentf" demi/emacs-cache-directory))



  (setq-default
   ;; Basics
   ring-bell-function    'ignore
   user-full-name        "Samantha Demi"
   user-mail-address     "hello@pewpewthespells.com"

   ;; Locations
   custom-file                demi/customize-file
   save-completions-file-name demi/save-completions-file-name
   save-place-file            demi/save-place-file
   abbrev-file-name           demi/abbrev-file-name
   diary-file                 demi/diary-file
   recentf-save-file          demi/recentf-save-file

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

  (rainbow-delimiters-mode nil)
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
  (spacemacs/toggle-fullscreen-frame-on)
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

(defun demi/user-config/hooks ()
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode)
  )

(defun dotspacemacs/user-config ()
  (demi/user-config/defaults)
  (demi/user-config/interface)
  (demi/user-config/theme)
  (demi/user-config/hooks)
  )

(with-eval-after-load 'org

  ;; Additional Imports
  (require 'calfw)
  (require 'calfw-cal)
  (require 'calfw-ical)
  (require 'calfw-org)

  ;; Add custom keywords
  (setq org-todo-keywords '(
    ;; Template: (sequence "ACTIONABLE_STATE(abbrev)" "|" "COMPLETION_STATE(abbrev)")

    ;; General Tasks TODO Keywords
    (sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "FOLLOWUP(f)" "|" "DONE(d)" "CANCELED(c)")

    ;; Meeting/Event TODO Keywords
    (sequence                                    "TENTATIVE(m)" "|" "CONFIRMED(y)" "DECLINED(n)" "CANCELED(c)")

    ;; Appointment TODO Keywords
    (sequence                          "ASAP(a)" "REQUESTED(r)" "|" "BOOKED(b)" "CANCELED(c)")

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


  ;; Custom Tags for Org-mode headings
  (setq org-tag-alist
    '(
      (:startgroup . nil)                ; Opening marker of a group of tags
      ("CURRENT" . nil) (:newline . nil) ; Tag item followed by indicator for this tag to insert a newline after it
      ("ONGOING" . nil) (:newline . nil) ; ^^^ ditto
      (:endgroup . nil)                  ; Closing marker of a group of tags
      )
    )

  ;; Custom Tag Faces
  (setq org-tag-faces
    '(
      ("CURRENT" . org-done) ; Using pre-defined faces, follows same template as the keyword faces above.
      ("ONGOING" . org-done)
      )
    )

  ;; Normally tags will be inherited by all sub-headings, disable that in these cases.
  (setq org-tags-exclude-from-inheritance
    '(
      "ONGOING"
      "CURRENT"
      )
    )

  ;; Insert date+time of marking a TODO item to a completed state.
  (setq org-log-done 'time)

  ;; If a TODO item is altered from a completed_state to an actionable_state, preserve the "CLOSED:" entry it has
  (setq org-closed-keep-when-no-todo t)

  ;; Set path to the default directory that Org-mode files should be found in.
  (setq org-directory (expand-file-name "~/org"))
  ;; Set the search pattern for generating Org-mode's Agenda.
  (setq org-agenda-files (directory-files org-directory t "^project\-.*\.org$"))

  ;; Disable for now, this needs more work.
  ;; The intent is to automatically export an html copy of the .org file, (as well as backing it up to iCloud+WebDAV) on save. this should be an async on-save hook only when Org-mode is active.
  ;(defun save-and-export-org-mode()
  ;  (when (eq major-mode 'org-mode) (org-html-export-to-html t t))
  ;  )
  ;(add-hook 'after-save-hook 'save-and-export-org-mode)
  )


;(with-eval-after-load "ispell"
;  (setq ispell-program-name "hunspell")
;  (setq ispell-really-hunspell t)
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
;  (ispell-set-spellchecker-params)
;  (ispell-hunspell-add-multi-dic "en_US,en_GB,Medical,hyphen_en_US,hyph_en_GB")
;  (setq ispell-dictionary "en_US") ;,en_GB,Medical,hyphen_en_US,hyph_en_GB")
;  )
