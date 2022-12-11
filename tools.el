(use-package which-key)
(require 'which-key)
(which-key-mode)

(setq dired-deletion-confirmer #'y-or-n-p)

(use-package projectile)
(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package rg)


					; key bindings
;; notes
(global-set-key (kbd "C-c n f") #'consult-notes)
(global-set-key (kbd "C-c n d") #'denote)
(global-set-key (kbd "C-c n D") #'denote-subdirectory)
(global-set-key (kbd "C-c n l") #'denote-link)
(global-set-key (kbd "C-c n t") #'denote-keywords-add)
(global-set-key (kbd "C-c n T") #'denote-keywords-remove)
(global-set-key (kbd "C-c n j") #'org-journal-new-entry)

;; buffer
(global-set-key (kbd "C-c b p") #'previous-buffer)
(global-set-key (kbd "C-c b n") #'next-buffer)
(global-set-key (kbd "C-c b k") #'kill-current-buffer)
(global-set-key (kbd "C-c b s") #'basic-save-buffer)
(global-set-key (kbd "C-c b r") #'revert-buffer)
(global-set-key (kbd "C-c b b") #'consult-buffer)
(global-set-key (kbd "C-c b x") #'scratch-buffer)
;; bookmark
(global-set-key (kbd "C-c <RET>") #'consult-bookmark)
(global-set-key (kbd "C-c b m") #'bookmark-set)
;; file
(global-set-key (kbd "C-c f r") #'consult-recent-file)

