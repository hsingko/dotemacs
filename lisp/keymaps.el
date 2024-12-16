;; unmap default
;; (global-unset-key (kbd "M-j")) ;; default-indent-new-line
;; (global-unset-key (kbd "M-k")) ;; forward-word

(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "S-<delete>"))
;; embrace the power of avy!
(global-set-key (kbd "M-j") #'avy-goto-char-timer)
(global-set-key (kbd "M-c") #'avy-goto-char-2)
;; (global-unset-key (kbd "C-\\"))
;; (global-set-key (kbd "M-m") #'toggle-input-method)


;; (global-set-key (kbd "C-c w h") #'split-window-horizontally)
;; (global-set-key (kbd "C-c w v") #'split-window-vertically)
;; (global-set-key (kbd "C-c w c") #'delete-window)
;; (global-set-key (kbd "C-c w C") #'delete-other-windows)

;; winner mode
(winner-mode)
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w r") #'winner-redo)

;; wrap words
;; see: https://stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs
;; (global-set-key (kbd "M-[") 'insert-pair)
;; (global-set-key (kbd "M-{") 'insert-pair)
;; (global-set-key (kbd "M-\"") 'insert-pair)
;; (global-set-key (kbd "M-(") 'insert-pair)
;; 以上代码作废，因为可以直接在 meow 的 keypad 模式中输入 pair
;; 参考：https://emacs-china.org/t/meow-smartparens/22729/5


;;helpful
(use-package helpful
  :commands describe-function)

(global-unset-key (kbd "C-h C-f")) ;unbind emacs-faq, good for meow keypad
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-functionv)
(global-set-key (kbd "C-h C") #'helpful-command)

;; key bindings
;; notes
;; (global-set-key (kbd "C-c n f") #'consult-notes)



(defvar-keymap hsk/notetaking-map
  :doc "My key map related to notetaking"
  ;; "f" #'consult-denote
  "c" #'org-capture
  "f" #'consult-denote-pinyin
  "d" #'denote
  "r" #'denote-rename-file-using-front-matter
  "t" #'denote-rename-file-keywords
  "j" #'org-journal-new-entry
  "z" #'olivetti-mode
  "q" #'calibredb
  "b" #'denote-show-backlinks-buffer
  "p" #'denote-type
  "l" #'consult-denote-link
  "h" #'consult-hugo-blog
  "x" #'xeft)

(keymap-set global-map "C-c n" hsk/notetaking-map)


(defvar-keymap hsk/buffer-opt-map
  :doc "buffer related key map"
  "p" #'previous-buffer
  "n" #'next-buffer
  "k" #'kill-current-buffer
  "s" #'basic-save-buffer
  "x" #'scratch-buffer)
(keymap-global-set "C-c b" hsk/buffer-opt-map)

;; bookmark
;; (global-set-key (kbd "C-c <RET>") #'consult-bookmark) 
(global-set-key (kbd "C-c b m") #'bookmark-set)
;; file
(defvar-keymap hsk/file-opt-map
  :doc "file associated operations"
  ;; "r" #'consult-recent-file
  ;; "f" #'find-file
  "U" #'doom/sudo-this-file
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



(keymap-global-set "C-x C-b" #'consult-buffer)
(keymap-global-set "C-c q t" #'modus-themes-toggle)
(keymap-global-set "C-c q d" #'+zoxide-cd)

;; org-fc
(defvar-keymap org-fc-keymap
  :doc "org-fc keymaps"
  "r" #'org-fc-review-buffer
  "R" #'org-fc-review-all
  "n" #'org-fc-type-normal-init
  "c" #'org-fc-type-cloze-init)
(keymap-global-set "C-c h" org-fc-keymap)


(keymap-global-set "C-x C-n" #'next-buffer)
(keymap-global-set "C-x C-p" #'previous-buffer)

(keymap-unset emacs-lisp-mode-map "C-c C-f")

(keymap-global-set "M-s p" #'consult-recent-file)

(provide 'keymaps)

