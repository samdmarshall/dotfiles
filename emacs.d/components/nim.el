(use-package nim-mode
  :after (company flycheck)
  :config
  (setq nimsuggest-path (expand-file-name "~/.nimble/bin/nimsuggest"))
  :hook (
	 (nim-mode . nimsuggest-mode)
	 (nimsuggest-mode . company-mode)
	 (nimsuggest-mode . flycheck-mode))
  )
