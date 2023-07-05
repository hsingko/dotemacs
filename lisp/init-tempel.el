(use-package tempel
  :bind
  (("M-+" . tempel-expand)
   ("M-*" . tempel-insert))
  :config
  (setq tempel-path (expand-file-name "templates" user-emacs-directory))
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (global-tempel-abbrev-mode)
  )

(use-package tempel-collection
  :after tempel)

(provide 'init-tempel)
