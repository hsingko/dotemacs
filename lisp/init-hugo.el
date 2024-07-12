(defun )

(defun move-and-link-image ()
  (interactive)
  (let (file filename)
    (setq file (read-file-name "Choose image:" "~/Pictures/"))
    (setq filename (file-name-nondirectory file))
    (rename-file file
		 (file-name-directory (buffer-file-name)))
    (insert (format "![](%s)" filename))))


(provide 'init-hugo)
