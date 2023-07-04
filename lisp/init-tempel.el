(use-package tempel
  :bind
  (("M-+" . tempel-expand)
   ("M-*" . tempel-insert))
  :config
  (setq tempel-path (expand-file-name "templates" user-emacs-directory)))

(use-package tempel-collection
  :after tempel)

(provide 'init-tempel)

