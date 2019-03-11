;; Custom Mode Line
(setq-default mode-line-format
  (list
    ; directory and buffer/file name
    ;'(:propertize (:eval (shorten-directory default-directory 30)) face mode-line-folder-face)
    '(:propertize "%b" face mode-line-filename-face)
    ; narrow [default -- keep?]
    " %n "
    ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
    '(vc-mode vc-mode)
		"  "
		; what are the active modes
		"[%[" '(:propertize mode-name face mode-line-mode-face) "%]]"
		"  "
		'(:eval (current-buffer-count-characters))
		"  "
		'(:propertize mode-line-process face mode-line-process-face)
    '(global-mode-string global-mode-string)
    "   "
  )
)

(defun current-buffer-count-characters ()
	(interactive)
	(format " @ %s characters" (buffer-size))
)

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
;(make-face 'mode-line-80col-face)

(force-mode-line-update t)
