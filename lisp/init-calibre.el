(use-package calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/data/CalibreBooks/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-id-width 4)
  (advice-add 'calibredb :after #'+disable-line-wrap-and-number)
  )

(defun +disable-line-wrap-and-number ()
  (visual-line-mode -1))

(provide 'init-calibre)
