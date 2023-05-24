(use-package color-rg
  :load-path "git/color-rg"
  :bind
  (("C-c f g" . color-rg-search-input)
   ("C-c f s" . color-rg-search-symbol)))

(provide 'init-rg)
