(use-package yasnippet
  :hook
  (org-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yas)
