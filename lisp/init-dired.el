(defun hide-dired-details-include-all-subdir-paths ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward dired-subdir-regexp nil t)
      (let* ((match-bounds (cons (match-beginning 1) (match-end 1)))
             (path (file-name-directory (buffer-substring (car match-bounds)
                                                          (cdr match-bounds))))
             (path-start (car match-bounds))
             (path-end (+ (car match-bounds) (length path)))
             (inhibit-read-only t))
        (put-text-property path-start path-end
                           'invisible 'dired-hide-details-information)))))

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
  (dired-mode . dired-hide-details-mode)
  (dired-after-readin . hide-dired-details-include-all-subdir-paths);;https://xenodium.com/hide-another-detail/
  ;; this feature is merged into dired.el, so in next release, I may not need this patch anymore
  )


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
  :init
  (defun +zoxide-cd ()
	(interactive)
	(cd (completing-read "path:" (zoxide-query) nil t)))
  :config
  (defun dired-jump-with-zoxide ()
	(interactive)
	(if (equal current-prefix-arg nil)
		(zoxide-open-with nil (lambda (file) (dired file)) t)
	  (zoxide-open-with nil (lambda (file) (dired-other-window file)) t)))
  :bind
  (:map dired-mode-map
		("P" . dired-jump-with-zoxide)))


(use-package dired-preview
  :config
  (setq dired-preview-delay 0.1)
  ;; (dired-preview-global-mode)
  )





(provide 'init-dired)
