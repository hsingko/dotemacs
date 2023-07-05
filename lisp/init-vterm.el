(use-package vterm
  :commands (vterm vterm-toggle-cd)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\*vterm\\*"
		 (display-buffer-in-side-window)
		 (window-height . 0.25)
		 (side . bottom))))

(use-package vterm-toggle
  :bind
  (("C-c C-k" . vterm-toggle-cd)))

(provide 'init-vterm)
