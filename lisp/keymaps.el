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



(defvar-keymap hsk/notetaking-map
  :doc "My key map related to notetaking"
  "f" #'denote-open-or-create
  "d" #'denote
  "t" #'denote-keywords-add
  "T" #'denote-keywords-remove
  "j" #'org-journal-new-entry
  "z" #'olivetti-mode
  "q" #'calibredb
  "b" #'denote-show-backlinks-buffer
  "x" #'xeft)

(keymap-set global-map "C-c n" hsk/notetaking-map)


(defvar-keymap hsk/buffer-opt-map
  :doc "buffer related key map"
  "p" #'previous-buffer
  "n" #'next-buffer
  "k" #'kill-current-buffer
  "s" #'basic-save-buffer
  "b" #'consult-buffer
  "x" #'scratch-buffer)
(keymap-global-set "C-c b" hsk/buffer-opt-map)

;; bookmark
(global-set-key (kbd "C-c <RET>") #'consult-bookmark)
(global-set-key (kbd "C-c b m") #'bookmark-set)
;; file
;; (global-set-key (kbd "C-c f r") #'consult-recent-file)
;; (global-set-key (kbd "C-c f f") #'find-file)
;; ;; (global-set-key (kbd "C-c f i") #'consult-imenu)
;; (global-set-key (kbd "C-c f i") #'hsk/consult-imenu-respect-narrow)
;; (global-set-key (kbd "C-c f w") #'widen)
;; (global-set-key (kbd "C-c f n") #'org-narrow-to-subtree)
;; (global-set-key (kbd "C-c f p") #'project-find-file)
;; (global-set-key (kbd "C-c f D") #'+delete-this-file)
;; (global-set-key (kbd "C-c f F") #'find-file-other-window)
;; (global-set-key (kbd "C-c f R") #'doom/move-this-file)
;; (global-set-key (kbd "C-c f U") #'doom/sudo-this-file)
;; (global-set-key (kbd "C-c f c") #'doom/copy-this-file)
;; (global-set-key (kbd "C-c f m") #'format-all-region-or-buffer)
;; (global-set-key (kbd "C-c f l") #'+insert-org-link-in-current-directory)

(defvar-keymap hsk/file-opt-map
  :doc "file associated operations"
  "r" #'consult-recent-file
  "f" #'find-file
  "i" #'hsk/consult-imenu-respect-narrow
  "w" #'widen
  "n" #'org-narrow-to-subtree
  "p" #'project-find-file
  "D" #'+delete-this-file
  "m" #'format-all-region-or-buffer)

(keymap-global-set "C-c f" hsk/file-opt-map)


;;utils
(defvar-keymap hsk/quick-utils-map
  :doc "my quick utils"
  "c" #'quick-calc
  "e" #'emoji-search)

(keymap-global-set "C-c q" hsk/quick-utils-map)


;; agenda
(global-set-key (kbd "C-c o") #'org-agenda)

(global-set-key (kbd "C-SPC") #'set-mark-command)

(provide 'keymaps)
