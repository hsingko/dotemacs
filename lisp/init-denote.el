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
                                  denote-directory))
  ;; :config
  (setq denote-excluded-directories-regexp "archive")
  (add-hook 'dired-mode-hook #'my/denote-dired-mode-hook))

(defun +create-free-writing (title)
  (interactive "sWhat's in your mind? ")
  (denote
   title nil nil (expand-file-name "freewriting" denote-directory)))


(use-package consult-notes
  :commands consult-notes
  :config
  (consult-notes-denote-mode)
  (setq consult-notes-denote-dir nil
	consult-notes-denote-display-id nil))


(provide 'init-denote)
