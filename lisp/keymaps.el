;; unmap default
(global-unset-key (kbd "M-j")) ;; default-indent-new-line
(global-unset-key (kbd "M-k")) ;; forward-word
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "S-<delete>"))
(global-set-key (kbd "M-p") #'pop-global-mark)
;; embrace the power of avy!
(global-set-key (kbd "M-j") #'avy-goto-char-timer)
(global-set-key (kbd "M-c") #'avy-goto-char-2)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "M-m") #'toggle-input-method)


(global-set-key (kbd "C-c w h") #'split-window-horizontally)
(global-set-key (kbd "C-c w v") #'split-window-vertically)
(global-set-key (kbd "C-c w c") #'delete-window)
(global-set-key (kbd "C-c w C") #'delete-other-windows)
;; winner mode
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w r") #'winner-redo)
;; wrap words
;; see: https://stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-(") 'insert-pair)


;; mark
(global-set-key (kbd "M-k") #'pop-global-mark)

;;helpful
(use-package helpful
  :commands describe-function)

(global-unset-key (kbd "C-h C-f")) ;unbind emacs-faq, good for meow keypad
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; key bindings
;; notes
;; (global-set-key (kbd "C-c n f") #'consult-notes)
(global-set-key (kbd "C-c n f") #'denote-open-or-create)
(global-set-key (kbd "C-c n d") #'denote)
(global-set-key (kbd "C-c n D") #'denote-subdirectory)
(global-set-key (kbd "C-c n l") #'denote-link)
(global-set-key (kbd "C-c n t") #'denote-keywords-add)
(global-set-key (kbd "C-c n T") #'denote-keywords-remove)
(global-set-key (kbd "C-c n j") #'org-journal-new-entry)
(global-set-key (kbd "C-c n c") #'org-capture)
(global-set-key (kbd "C-c n z") #'olivetti-mode)
(global-set-key (kbd "C-c n p") #'org-download-clipboard)
(global-set-key (kbd "C-c n q") #'calibredb)
(global-set-key (kbd "C-c n m") #'list-denotes)
(global-set-key (kbd "C-c n b") #'denote-link-show-backlinks-buffer)
(global-set-key (kbd "C-c n r") #'denote-rename-file-using-front-matter)
(global-set-key (kbd "C-c n s") #'denote-signature)
(global-set-key (kbd "C-c n o") #'denote-open-or-create)
;; buffer
(global-set-key (kbd "C-c b p") #'previous-buffer)
(global-set-key (kbd "C-c b n") #'next-buffer)
(global-set-key (kbd "C-c b k") #'kill-current-buffer)
(defun +close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(defun +save-all-buffers ()
  (interactive)
  (save-some-buffers t))
(global-set-key (kbd "C-c b K") #'+close-all-buffers)
(global-set-key (kbd "C-c b s") #'basic-save-buffer)
(global-set-key (kbd "C-c b r") #'revert-buffer)
(global-set-key (kbd "C-c b b") #'consult-buffer)
(global-set-key (kbd "C-c b x") #'scratch-buffer) ; emacs-29
(global-set-key (kbd "C-c b S") #'save-some-buffers)

;; bookmark
(global-set-key (kbd "C-c <RET>") #'consult-bookmark)
(global-set-key (kbd "C-c b m") #'bookmark-set)
;; file
(global-set-key (kbd "C-c f r") #'consult-recent-file)
(global-set-key (kbd "C-c f f") #'find-file)
;; (global-set-key (kbd "C-c f i") #'consult-imenu)
(global-set-key (kbd "C-c f i") #'+consult-imenu-respect-narrow)
(global-set-key (kbd "C-c f w") #'widen)
(global-set-key (kbd "C-c f n") #'org-narrow-to-subtree)
(global-set-key (kbd "C-c f p") #'project-find-file)
(global-set-key (kbd "C-c f D") #'+delete-this-file)
(global-set-key (kbd "C-c f F") #'find-file-other-window)
(global-set-key (kbd "C-c f R") #'doom/move-this-file)
(global-set-key (kbd "C-c f U") #'doom/sudo-this-file)
(global-set-key (kbd "C-c f c") #'doom/copy-this-file)
(global-set-key (kbd "C-c f m") #'format-all-region-or-buffer)
(global-set-key (kbd "C-c f l") #'+insert-org-link-in-current-directory)
(global-set-key (kbd "C-c f s") #'sdcv-search-input)
;;utils
(global-set-key (kbd "C-c q c") #'quick-calc)
(global-set-key (kbd "C-c q e") #'emoji-search)


;; agenda
(global-set-key (kbd "C-c o") #'org-agenda)

(global-set-key (kbd "C-SPC") #'set-mark-command)

(provide 'keymaps)
