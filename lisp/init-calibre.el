(use-package calibredb
  ;; :commands (calibredb)
  :config
  (setq calibredb-preferred-format 'epub)
  (setq calibredb-root-dir "~/data/CalibreBooks/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-id-width 6)
  (advice-add 'calibredb :after #'+disable-line-wrap-and-number))

(defun +disable-line-wrap-and-number ()
  (visual-line-mode -1))

;; (use-package nov
;;   :mode ("\\.epub\\'" . nov-mode)
;;   ;; :config
;;   ;; (add-to-list 'auto-mode-alist
;;   ;; 	       '("\\.epub\\'" . nov-mode))
;;   )

(defun my/calibredb-consult-read (arg)
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
