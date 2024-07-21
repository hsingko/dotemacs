(use-package calibredb
  :commands (calibredb)
  :bind
  (:map calibredb-search-mode-map
		("C-v" . calibredb-search-next-page)
		("M-v" . calibredb-search-previous-page))
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


(defun hsk/calibredb-consult-open (arg)
  "open books in calibredb using default tool"
  (interactive "P")
  (let* ((cand (let ((candidates calibredb-search-entries))
		 (consult--read candidates
				:prompt "Pick a book: "
				:lookup #'consult--lookup-cdr
				:sort nil)))
	 (fp (cadr (assoc :file-path (car cand)))))
    (calibredb-open-with-default-tool fp)))

(defun hsk/gen-ebook-todo-toc ()
  (interactive)
  (let* ((cand (let ((candidates calibredb-search-entries))
		 (consult--read candidates
				:prompt "Pick a book: "
				:lookup #'consult--lookup-cdr
				:sort nil)))
	 (title (cadr (assoc :book-title (car cand))))
	 (fp (car (split-string (cadr (assoc :file-path (car cand))) ",")))
	 (formats (cadr (assoc :file-path (car cand))))
	 (epub-fp (cond ((string-match-p "epub" fp) fp)
			((string-match-p "epub" formats)
			 (s-concat
			  (car (split-string fp "."))
			  ".epub"))
			;; ((string-match-p "mobi\\|azw" formats)
			;;  (let ((tmp (make-temp-file "ebook-convert-" nil ".epub" nil)))
			;;    (call-process-shell-command (format "ebook-convert %s %s" fp tmp))
			;;    tmp))
			(t
			 (error "epub not supported for this book")))))
    (insert (format "* TODO [0%] %s\n" title))
    (insert (shell-command-to-string (format "python ~/Lab/ebook-toc.py \"%s\"" (file-truename
										 epub-fp))))))

(provide 'init-calibre)
