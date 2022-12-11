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
           (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
      (progn
        (delete-region (org-entry-beginning-position) (org-entry-end-position))
        (denote heading (org-get-tags) 'org)
        (insert text))
    (user-error "No subtree to extract; aborting")))


;; (require 'denote-org-dblock)

(use-package denote
  :init
  (setq denote-directory "~/Documents/org/notes"
        denote-file-type nil
        denote-dired-directories (list
                                   denote-directory
                                   (thread-last denote-directory (expand-file-name "movies"))
                                   (thread-last denote-directory (expand-file-name "books"))
                                   (thread-last denote-directory (expand-file-name "dailies"))
				   (thread-last denote-directory (expand-file-name "courses"))
                                   (thread-last denote-directory (expand-file-name "posts"))
                                   (thread-last denote-directory (expand-file-name "logs"))
                                   (thread-last denote-directory (expand-file-name "attachments"))
                                   ))
  ;; :config
  (add-hook 'dired-mode-hook #'my/denote-dired-mode-hook))



(use-package consult-notes
  :after denote
  :commands (consult-notes
         consult-notes-search-in-all-notes)
  :init
  ;; :config
  (setq consult-notes-sources '(
                                ("Nosleep"  ?s  "~/Documents/org/nosleep")
                                )) ;; Set notes dir(s), see below
  ;; Set org-roam integration OR denote integration
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  )
