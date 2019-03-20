
(use-package org
  :config
  (setq initial-major-mode 'org-mode)
  (setq org-todo-keywords '(
			    (sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "FOLLOWUP(f)" "|" "DONE(d)" "CANCELED(c)")
			    (sequence "TENTATIVE(m)" "|" "CONFIRMED(y)" "DECLINED(n)" "CANCELED(c)")
			    (sequence "ASAP(a)" "REQUESTED(r)" "|" "BOOKED(b)" "CANCELED(c)")
			    (sequence "|" "CURRENT" "ONGOING")
			    ))
  
  (setq org-todo-keyword-faces '(
				 ("TODO" . org-todo)
				 ("STARTED" . (:foreground "orange" :weight bold :background "yellow" :box (:line-width 1)))
				 ("WAITING" . (:foreground "blue" :weight bold :background "cyan" :box (:line-width 1)))
				 ("FOLLOWUP" . (:foreground "red" :weight bold :background "orange" :box (:line-width 1)))
				 ("DONE" . org-done)
				 ("CANCELED" . (:foreground "grey" :weight bold :background "white" :box (:line-width 1)))

				 ("TENTATIVE" . (:foreground "orange" :weight bold :background "yellow" :box (:line-width 1)))
				 ("CONFIRMED" . org-done)
				 ("DECLINED" . (:foreground "black" :weight bold :background "grey" :box (:line-width 1)))
				 ("CANCELED" . (:foreground "grey" :weight bold :background "white" :box (:line-width 1)))

				 ("ASAP" . org-todo)
				 ("REQUESTED" . (:foreground "orange" :weight bold :background "yellow" :box (:line-width 1)))
				 ("BOOKED" . org-done)
				 ("CANCELED" . (:foreground "grey" :weight bold :background "white" :box (:line-width 1)))

				 ("CURRENT" . org-done)
				 ("ONGOING" . org-done)
				 ))
  
  (setq org-log-done 'time)
  (setq org-closed-keep-when-no-todo t)
  (setq org-directory (expand-file-name "~/org"))
  (setq org-agenda-files (directory-files org-directory t "^project\-.*\.org$"))

  (defun save-and-export-org-mode()
    (when (eq major-mode 'org-mode) (org-html-export-to-html t t))
  )
  (add-hook 'after-save-hook 'save-and-export-org-mode)
)

(use-package calfw
  :after (org)
  :config
  (setq calendar-week-start-day 1)
  )
(use-package calfw-cal
  :after (calfw)
  )
(use-package calfw-ical
  :after (calfw-cal)
  )
(use-package calfw-org
  :after (calfw)
  )
