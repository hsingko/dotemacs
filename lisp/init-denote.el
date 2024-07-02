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
	denote-silo-extras-directories (list (expand-file-name "secret/" org-directory))
        denote-dired-directories (list
                                  denote-directory
				  "~/Pictures/"))

  ;; :config
  (setq denote-excluded-directories-regexp "archive")
  (denote-rename-buffer-mode 1)
  (setq denote-save-buffers t)
  (setq denote-rename-no-confirm t)
  (setq denote-rename-buffer-format "[D] %t")
  (setq denote-backlinks-show-context t)
  (setq xref-search-program 'ugrep)
  (add-hook 'dired-mode-hook #'my/denote-dired-mode-hook)
  ;; add *.org.age note type, for security
  (add-to-list 'denote-file-types
	       '(age
		 :extension ".org.age"
		 :date-function denote-date-org-timestamp
		 :front-matter denote-org-front-matter
		 :title-key-regexp "^#\\+title\\s-*:"
		 :title-value-function identity
		 :title-value-reverse-function denote-trim-whitespace
		 :keywords-key-regexp "^#\\+filetags\\s-*:"
		 :keywords-value-function denote-format-keywords-for-org-front-matter
		 :keywords-value-reverse-function denote-extract-keywords-from-front-matter
		 :link denote-org-link-format
		 :link-in-context-regexp denote-org-link-in-context-regexp))
  :config
  (require 'denote-org-extras)
  ;; (require 'denote-silo-extras)
  )

(defun +create-free-writing (title)
  (interactive "sWhat's in your mind? ")
  (denote
   title nil nil (expand-file-name "freewriting" denote-directory)))


(provide 'init-denote)
