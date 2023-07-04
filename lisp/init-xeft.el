(use-package xeft
  :config
  (setq xeft-directory (expand-file-name "wiki" org-directory)
	xeft-recursive t)
  (setq xeft-default-extension "org")
  (add-to-list 'xeft-ignore-extension "jpg")
  (add-to-list 'xeft-ignore-extension "svg")
  (add-to-list 'xeft-ignore-extension "png")
  (defun my/xeft-create-note ()
    (interactive)
    (let* ((search-phrase (xeft--get-search-phrase))
           (file-name (funcall xeft-filename-fn search-phrase))
           (file-path (expand-file-name file-name xeft-directory))
           (exists-p (file-exists-p file-path)))
      (when (or (search-forward "Press RET to create a new note" nil t)
		(y-or-n-p (format "Create file `%s'? " file-name)))
	(find-file file-path)
	(unless exists-p
          (save-buffer)
          (xeft--front-page-cache-refresh))
	(run-hooks 'xeft-find-file-hook))))
  :bind
  (("C-c n x" . xeft))
  (:map xeft-mode-map
	([remap xeft-create-note] . my/xeft-create-note)))

(provide 'init-xeft)
