(defun my/denote-dired-mode-hook()
  (denote-dired-mode-in-directories)
  (when denote-dired-mode
    (dired-hide-details-mode +1)
    (dired-omit-mode)))

(use-package denote
  :commands denote
  :init
  (setq denote-directory (expand-file-name "notes" org-directory)
        denote-file-type "org"
        denote-dired-directories (list
                                  denote-directory
                                  ;; (thread-last denote-directory (expand-file-name "movies"))
                                  ;; (thread-last denote-directory (expand-file-name "books"))
				  ;; (thread-last denote-directory (expand-file-name "courses"))
                                  ;; (thread-last denote-directory (expand-file-name "posts"))
                                  ;; (thread-last denote-directory (expand-file-name "logs"))
                                  ))
  ;; :config
  (setq denote-excluded-directories-regexp "archive")
  (add-hook 'dired-mode-hook #'my/denote-dired-mode-hook))

(defun +create-free-writing (title)
  (interactive "sWhat's in your mind? ")
  (denote
   title nil nil (expand-file-name "freewriting" denote-directory)))


(use-package consult-notes
  :config
  (consult-notes-denote-mode))


(provide 'init-denote)
