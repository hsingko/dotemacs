(require 'denote-org-dblock)
(defun my/denote-dired-mode-hook()
  (denote-dired-mode-in-directories)
  (when denote-dired-mode
      (dired-hide-details-mode +1)))

(defun my/denote-create-new-note-from-region (beg end)
  "Create note whose contents include the text between BEG and END.
Prompt for title and keywords of the new note."
  (interactive "r")
  (if-let (((region-active-p))
           (text (buffer-substring-no-properties beg end)))
      (progn
        (denote (denote--title-prompt) (denote--keywords-prompt))
        (insert text))
    (user-error "No region is available")))

(defun my/denote-org-extract-subtree ()
  "Create new Denote note using current Org subtree.
Make the new note use the Org file type, regardless of the value
of `denote-file-type'.

Use the subtree title as the note's title.  If available, use the
tags of the heading are used as note keywords.

Delete the original subtree."
  (interactive)
  (if-let ((text (org-get-entry))
           (heading-text (org-get-heading :no-tags :no-todo :no-priority :no-comment))
	   (heading (org-get-heading :no-tags))
	   )
      (progn
					; code blog contains bug, sometimes can't delete sub-subtree
        (delete-region (org-entry-beginning-position) (org-entry-end-position))
        (denote heading-text (org-get-tags) 'org
		(thread-last denote-directory (expand-file-name "posts"))
		)
	;; (insert "* ")
	;; (insert heading)
	;; (insert "\n")
	(insert (format "* %s \n" heading))
        (insert text))
    (user-error "No subtree to extract; aborting")))


;; (require 'denote-org-dblock)

(use-package denote
  :commands denote
  :init
  (setq denote-directory (expand-file-name "notes" org-directory)
        denote-file-type nil
        denote-dired-directories (list
                                  denote-directory
                                  (thread-last denote-directory (expand-file-name "movies"))
                                  (thread-last denote-directory (expand-file-name "books"))
				  (thread-last denote-directory (expand-file-name "courses"))
                                  (thread-last denote-directory (expand-file-name "posts"))
                                  (thread-last denote-directory (expand-file-name "logs"))
                                  ))
  ;; :config
  (add-hook 'dired-mode-hook #'my/denote-dired-mode-hook))

(defun +create-free-writing (title)
  (interactive "sWhat's in your mind? ")
  (denote
   title nil nil (expand-file-name "freewriting" denote-directory)))


(use-package denote-menu
  :after denote
  :load-path "git/denote-menu"
  :config
  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired)
  )
