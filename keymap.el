;; unmap default
(global-unset-key (kbd "M-j")) ;; default-indent-new-line
(global-unset-key (kbd "M-k")) ;; forward-word
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "S-<delete>"))
(global-set-key (kbd "M-SPC") #'set-mark-command)
(global-set-key (kbd "M-p") #'pop-global-mark)
;; embrace the power of avy!
(global-set-key (kbd "M-j") #'avy-goto-char-timer)
(global-set-key (kbd "M-c") #'avy-goto-char-2)


;; window
(winner-mode)
(use-package ace-window)
(global-set-key (kbd "C-c w h") #'split-window-horizontally)
(global-set-key (kbd "C-c w v") #'split-window-vertically)
(global-set-key (kbd "C-c w c") #'delete-window)
(global-set-key (kbd "C-c w C") #'delete-other-windows)
(global-set-key (kbd "M-o") 'ace-window) 
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
  :commands describe-function
  )

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
(global-set-key (kbd "C-c n c") #'org-capture)
(global-set-key (kbd "C-c n z") #'olivetti-mode)
(global-set-key (kbd "C-c n p") #'org-download-clipboard)
(global-set-key (kbd "C-c n q") #'calibredb)
(global-set-key (kbd "C-c n m") #'list-denotes)
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
(global-set-key (kbd "C-c b x") #'scratch-buffer)
(global-set-key (kbd "C-c b S") #'+save-all-buffers)

;; bookmark
(global-set-key (kbd "C-c <RET>") #'consult-bookmark)
(global-set-key (kbd "C-c b m") #'bookmark-set)
;; file
(global-set-key (kbd "C-c f r") #'consult-recent-file)
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "C-c f f") #'find-file)
(global-set-key (kbd "C-c f w") #'+create-free-writing)
(global-set-key (kbd "C-c f p") #'+find-file-in-config)
(global-set-key (kbd "C-c f D") #'+delete-this-file)
(global-set-key (kbd "C-c f F") #'find-file-other-window)
(global-set-key (kbd "C-c f R") #'doom/move-this-file)
(global-set-key (kbd "C-c f U") #'doom/sudo-this-file)
(global-set-key (kbd "C-c f c") #'doom/copy-this-file)


;; agenda
(global-set-key (kbd "C-c o") #'org-agenda)


;; dired
(use-package dired
    :ensure nil
    :config
)
