(use-package recentf
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode 1))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))


(provide 'init-save)
