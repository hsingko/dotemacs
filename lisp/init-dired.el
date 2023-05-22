(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
	("h" . dired-up-directory)
	("l" . dired-find-file)
	)
  :config
  (setq dired-deletion-confirmer #'y-or-n-p)
  (setq delete-by-moving-to-trash t))

(provide 'init-dired)
