(use-package nov
  :config
  (setq nov-unzip-program (executable-find "bsdtar")
	nov-unzip-args '("-xC" directory "-f" filename))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  )

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Bookerly"
                                           :size 20))


  
(add-hook 'nov-mode-hook (lambda ()
			   (display-line-numbers-mode -1)))
