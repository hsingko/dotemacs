(use-package calibredb
  :commands (calibredb)
  :config
  (setq calibredb-preferred-format 'epub)
  (setq calibredb-root-dir "~/Documents/CalibreBooks/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-id-width 0)
  (set-face-attribute 'calibredb-search-header-highlight-face
		      nil :family "Iosevka Comfy")
  (set-face-attribute 'calibredb-format-face
		      nil :family "Iosevka Comfy")
  (set-face-attribute 'calibredb-title-face
		      nil :family "Iosevka Comfy")
  (set-face-attribute 'calibredb-highlight-face
		      nil :family "Iosevka Comfy")
  (set-face-attribute 'calibredb-date-face
		      nil :family "Iosevka Comfy")
  (set-face-attribute 'calibredb-archive-face
		      nil :family "Iosevka Comfy"))


(defun hsk/calibredb-consult-read (arg)
  "consult read for calibredb."
  (interactive "P")
  (let* ((cand (let ((candidates calibredb-search-entries))
		 (consult--read candidates
				:prompt "Pick a book: "
				:lookup #'consult--lookup-cdr
				:sort nil)))
	 (id (cadr (assoc :id (car cand))))
	 (title (cadr (assoc :book-title (car cand)))))
    (insert (format "[[calibredb:%s][%s]]" id title))))


(provide 'init-calibre)
