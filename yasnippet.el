(use-package yasnippet)
(use-package yasnippet-snippets)

(require 'yasnippet)
(yas-reload-all)
(add-hook 'org-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'yas-minor-mode)
