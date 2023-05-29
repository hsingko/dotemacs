; package management
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq custom-file "~/.emacs.d/custom.el")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
(require 'init-dashboard)
(require 'init-meow)
(require 'init-consult)
(require 'init-vertico)
(require 'init-marginalia)
(require 'init-utils)
(require 'init-markdown)
(require 'init-org)
(require 'init-denote)
(require 'keymaps)
(require 'init-cn)
(require 'init-calibre)
(require 'init-yas)
(require 'init-bridge)
(require 'init-dired)
(require 'init-rg)
(require 'init-embark)
(require 'init-builtin)
(require 'init-magit)
(require 'init-window)
(require 'init-helper)
(require 'init-vterm)
(require 'init-xeft)

(setq eww-retrieve-command '("readable"))
(put 'erase-buffer 'disabled nil)


;; isearch(incremental search)
;; https://www.youtube.com/watch?v=9CdbfZXsrqg
(setq-default isearch-lazy-count t)
;; make use of search ring, M-n, M-p to safari the ring
;; use C-w to yank word under the cursor, hit multiple time to yank more words
;; 在选择的时候可以直接用 M-% 对当前模式进行替换
(setq isearch-allow-motion t) ;; 通过 M-<, M-> 来跳到最初和最后的匹配项
;; 使用 M-s o 用当前模式开始 occur mode

(require 'server)
(unless (server-running-p)
  (server-start))

;; marks
(setq-default set-mark-command-repeat-pop t)
;; but I still don't know how to use mark
;; (global-unset-key (kbd "C-@"))
;; (global-set-key (kbd "M-SPC") 'set-mark-command)

(setq initial-scratch-message "")


;; undo redo
(global-unset-key (kbd "C-M-_"))
;; (global-set-key (kbd "C-r") #'undo-redo)



(defun clear-minibuffer ()
  "Clear the content of the minibuffer."
  (interactive)
  (when (active-minibuffer-window)
    (minibuffer-delete (window-buffer (active-minibuffer-window)))))

(define-key minibuffer-local-map (kbd "C-c c") 'clear-minibuffer)

;; (use-package auto-insert
;;   :ensure nil
;;   :config
;;   (setq auto-insert-directory (expand-file-name "templates"
;; 						user-emacs-directory))
;;   (setq auto-insert-query nil)
;;   (define-auto-insert "\\.org\\'" "org-template.org")
;;   (add-hook 'find-file-hook 'auto-insert))
