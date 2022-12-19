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

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "/data/Calibre Library/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  ;; (setq calibredb-library-alist '(("~/OneDrive/Org/Doc/Calibre")
  ;;                                 ("~/Documents/Books Library")
  ;;                                 ("~/Documents/LIB1")
  ;;                                 ("/Volumes/ShareDrive/Documents/Library/")))
  )


(defun +disable-line-wrap-and-number ()
  (visual-line-mode -1))

(advice-add 'calibredb :after #'+disable-line-wrap-and-number)
  
(add-hook 'nov-mode-hook (lambda ()
			   (display-line-numbers-mode -1)))
