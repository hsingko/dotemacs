
(use-package phi-search
  :config
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward)
  (global-set-key (kbd "M-%") 'phi-replace-query))

(provide 'init-phisearch)
