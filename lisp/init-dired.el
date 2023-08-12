(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
	("h" . dired-up-directory)
	("l" . dired-find-file)
	("N" . dired-narrow)
	("E" . calibredb-dired-add))
  :config
  (setq dired-bind-man nil)
  (setq dired-listing-switches "-alhv --group-directories-first")
  (setq delete-by-moving-to-trash t)
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t) ;; make dired guess the destnation by another dired buffer in split size window
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode))


(use-package dired-narrow)


(provide 'init-dired)
