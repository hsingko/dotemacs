(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          ;; "\\*Async Shell Command\\*"
          help-mode
	  denote-backlinks-mode
          compilation-mode))
  (setq popper-reference-buffers
      (append popper-reference-buffers
              '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                "^\\*term.*\\*$"   term-mode   ;term as a popup
                "^\\*eat.*\\*$"   eat-mode   ;term as a popup
                )))
  (popper-mode +1)
  (popper-echo-mode +1))

(provide 'init-window)
