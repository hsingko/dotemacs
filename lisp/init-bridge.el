(use-package lsp-bridge
  :load-path "git/lsp-bridge"
  :hook
  (prog-mode . lsp-bridge-mode)
  :config
  (setq acm-enable-yas nil
	acm-enable-tempel t))

(provide 'init-bridge)
