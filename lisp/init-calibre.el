(use-package calibredb
  :config
  (setq calibredb-preferred-format 'epub)
  (setq calibredb-root-dir "~/data/CalibreBooks/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-id-width 4)
  (advice-add 'calibredb :after #'+disable-line-wrap-and-number))

(defun +disable-line-wrap-and-number ()
  (visual-line-mode -1))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist
	       '("\\.epub\\'" . nov-mode)))

(provide 'init-calibre)
