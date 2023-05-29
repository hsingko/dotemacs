(use-package xeft
  :after denote
  :config
  (setq xeft-directory (expand-file-name "wiki" org-directory)
	xeft-recursive t)
  (setq xeft-default-extension "org")
  (add-to-list 'xeft-ignore-extension "jpg")
  (add-to-list 'xeft-ignore-extension "svg")
  (add-to-list 'xeft-ignore-extension "png")
  (global-set-key (kbd "C-c n x") #'xeft)
  )

(provide 'init-xeft)
