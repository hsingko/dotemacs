(defface consult-buku-id-face
  '((t :foreground "#3B6EA8"))
  "face for buku id")

(defface consult-buku-domain-face
  '((t :foreground "#3B6EA8"
       :family "Iosevka Comfy"))
  "face for buku domain")

(defface consult-buku-title-face
  '((t :family "Iosevka Comfy"))
  "face for buku domain")

(defface consult-buku-tag-face
  '((t :foreground "#996c4f"
       :family "Iosevka Comfy"))
  "face for buku tags")

(defun hsk/format-string-with-fixed-length (str len)
  (if (> (length str) len)
      (add-text-properties len (length str) '(display "...") str))
    str)


(defun consult-buku-handle-tags (str)
  (setq str (substring str 0 (1- (length str))))
  (if (> (length str) 1)
      (replace-regexp-in-string "," " #" str)
    ""))

(defun hsk/consult-buku--build-string (row)
  (let ((id (nth 0 row))
	(title (nth 1 row))
	(domain (url-host (url-generic-parse-url (nth 2 row))))
	(tags (nth 3 row)))
    (format "%-3s %-70s %-25s %-20s"
	    (propertize (number-to-string id) 'face 'consult-buku-id-face)
	    (propertize (hsk/format-string-with-fixed-length title 67) 'face 'consult-buku-title-face)
	    (propertize domain 'face 'consult-buku-domain-face)
	    (propertize (consult-buku-handle-tags tags)
			'face 'consult-buku-tag-face))))

(defun hsk/consult-buku--prepare-entries ()
  (let ((result (sqlite-select (sqlite-open buku-db-file)
				      "select id, metadata, url, tags from bookmarks")))
    (mapcar (lambda (row)
	      (cons (hsk/consult-buku--build-string row)
		    (nth 2 row)))
	    result)))


(defun hsk/consult-buku ()
  "consult buku bookmarks"
  (interactive)
  (browse-url (consult--read (hsk/consult-buku--prepare-entries)
			     :lookup #'consult--lookup-cdr
			     :sort nil)))

(keymap-global-set "M-s b" #'hsk/consult-buku)
