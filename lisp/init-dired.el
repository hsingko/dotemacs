(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
		("h" . dired-up-directory)
		("j" . dired-next-line)
		("k" . dired-previous-line)
		("l" . dired-find-file)
		("N" . dired-narrow)
		("E" . hsk/add-book-to-calibredb))
  :config
  (setq dired-bind-man nil)
  (setq dired-listing-switches "-alhv --group-directories-first")
  (setq delete-by-moving-to-trash t)
  (setq dired-auto-revert-buffer t)
  (setq dired-async-mode t)
  (setq dired-dwim-target t) ;; make dired guess the destnation by another dired buffer in split size window
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode))


(defun hsk/add-book-to-calibredb ()
  (interactive)
  (unless (featurep 'calibredb)
    (require 'calibredb))
  (calibredb-dired-add))


(use-package dired-narrow)
;; (use-package dired-subtree)

(use-package casual-dired
  :commands dired
  :bind (:map dired-mode-map ("C-o" . #'casual-dired-tmenu)))


(use-package zoxide
  :config
  (defun +zoxide-cd ()
	(interactive)
	(cd (completing-read "path:" (zoxide-query) nil t)))
  
  (defun dired-jump-with-zoxide (&optional other-window)
	(interactive "P")
	;; (zoxide-open-with nil (lambda (file) (dired-jump other-window file)) t)
	(zoxide-open-with nil (lambda (file) (dired file)) t)
	)
  :bind
  (:map dired-mode-map
		("P" . dired-jump-with-zoxide)))


(provide 'init-dired)
