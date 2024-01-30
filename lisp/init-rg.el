(use-package deadgrep
  :bind
  (("C-c f d" . deadgrep))
  (:map deadgrep-mode-map
	("e" . 'wgrep-change-to-wgrep-mode)))

(use-package wgrep-deadgrep)


(provide 'init-rg)
