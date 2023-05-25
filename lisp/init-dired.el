(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
	("h" . dired-up-directory)
	("l" . dired-find-file)
	)
  :config
  (setq dired-deletion-confirmer #'y-or-n-p)
  (setq dired-ls-sorting-switches "-alh")
  (setq delete-by-moving-to-trash t)
  :hook
  (dired-mode . dired-omit-mode))

(provide 'init-dired)
