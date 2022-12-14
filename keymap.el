;; unmap default
(global-unset-key (kbd "M-j")) ;; default-indent-new-line
(global-unset-key (kbd "M-k")) ;; forward-word


;; embrace the power of avy!
(global-set-key (kbd "M-j") #'avy-goto-char-timer)


;; mark
(global-set-key (kbd "M-k") #'pop-global-mark)

;;helpful
(use-package helpful)
(global-unset-key (kbd "C-h C-f")) ;unbind emacs-faq, good for meow keypad
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; key bindings
;; notes
(global-set-key (kbd "C-c n f") #'consult-denote)
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
(global-set-key (kbd "C-c b S") #'org-save-all-org-buffers)

;; bookmark
(global-set-key (kbd "C-c <RET>") #'consult-bookmark)
(global-set-key (kbd "C-c b m") #'bookmark-set)
;; file
(global-set-key (kbd "C-c f r") #'consult-recent-file)
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "C-c f f") #'find-file)
(global-set-key (kbd "C-c f p") #'+find-file-in-config)
(global-set-key (kbd "C-c f D") #'+delete-this-file)
(global-set-key (kbd "C-c f F") #'find-file-other-window)

;; agenda
(global-set-key (kbd "C-c o") #'org-agenda)
