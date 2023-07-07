(use-package lsp-bridge
  :load-path "git/lsp-bridge"
  :custom
  (acm-enable-yas nil)
  (acm-enable-tempel t)
  :hook
  (prog-mode . lsp-bridge-mode))

(provide 'init-bridge)
