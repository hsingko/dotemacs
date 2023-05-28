(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*e?shell\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom))
     ("2023.org"
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom))
     ("CAPTURE"
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)))))


(provide 'init-window)
