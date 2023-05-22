(use-package lsp-bridge
  :load-path "git/lsp-bridge"
  :hook
  (prog-mode . lsp-bridge-mode))

(provide 'init-bridge)
