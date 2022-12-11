(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-r"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-r") 'undo-fu-only-redo))

; extend undo limit href:https://github.com/emacsmirror/undo-fu
(setq undo-limit 6710886400) ;; 64mb.
(setq undo-strong-limit 100663296) ;; 96mb.
(setq undo-outer-limit 1006632960) ;; 960mb.




