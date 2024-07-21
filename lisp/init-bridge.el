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
  :config
  (add-to-list 'lsp-bridge-single-lang-server-mode-list
			 '(inferior-python-mode . lsp-bridge-python-lsp-server))
  :hook
  (prog-mode . lsp-bridge-mode))


(provide 'init-bridge)

