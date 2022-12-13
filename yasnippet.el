(use-package yasnippet)
(use-package yasnippet-snippets
  :after yasnippet
  )

(require 'yasnippet)
(yas-reload-all)
(add-hook 'org-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'yas-minor-mode)
