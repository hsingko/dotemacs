(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode)
  (add-hook 'html-mode-hook #'yas-minor-mode)
  )
(use-package yasnippet-snippets
  :after yasnippet
  )

