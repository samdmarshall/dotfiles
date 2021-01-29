
(defconst org-demi-packages
  '(
    calfw
    calfw-cal
    calfw-ical
    calfw-org

    org-caldav
    org-vcard
;;    org-mind-map
    org-if
    (org-books :location (recipe :fetcher github :repo "lepisma/org-books"))
    )
  )

;;(when (configuration-layer/layer-usedp 'org-demi)

  (defun org-demi/init-calfw ()
    (use-package calfw :config (setq calendar-week-start-day 1))
    )

  (defun org-demi/init-calfw-cal ()
    (use-package calfw-cal)
    )

  (defun org-demi/init-calfw-ical ()
    (use-package calfw-ical)
    )

  (defun org-demi/init-calfw-org ()
    (use-package calfw-org)
    )

  (defun org-demi/init-org-caldav ()
    (use-package org-caldav)
    )

  (defun org-demi/init-org-vcard ()
    (use-package org-vcard)
    )

;;  (defun org-demi/init-org-mind-map ()
;;    (use-package org-mind-map)
;;    )

  (defun org-demi/init-org-if ()
    (use-package org-if)
    )

  (defun org-demi/init-org-books ()
    (use-package org-books)
    )

;;  )
