(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package lsp-bridge
  :load-path "git/lsp-bridge"
  :custom
  (acm-enable-yas t)
  (lsp-bridge-enable-completion-in-minibuffer t)
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-org-babel t)
  :hook
  (prog-mode . lsp-bridge-mode))


(provide 'init-bridge)

