;; package management
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))
(setq url-proxy-services
      '(("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))

(setq use-package-enable-imenu-support t) ;; this line must placed before import `use-package`
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq custom-file "~/.emacs.d/custom.el")

;; change auto-save directory
;; I don't if it works
;; yes, it works
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/autosave/") t)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
(require 'init-meow)
(require 'init-consult)
(require 'init-vertico)
(require 'init-marginalia)
(require 'init-utils)
(require 'init-lang)
(require 'init-org)
(require 'init-denote)
(require 'keymaps)
(require 'init-cn)
(require 'init-calibre)
(require 'init-tempel)
(require 'init-dired)
(require 'init-rg)
(require 'init-embark)
(require 'init-builtin)
(require 'init-window)
(require 'init-helper)
;; (require 'init-vterm)
(require 'init-xeft)
(require 'init-html)
(require 'init-formater)
(require 'init-abbrev)
(require 'init-shellcmd)
(require 'init-corfu)
;; (require 'init-bridge)
(require 'init-magit)
(require 'init-undo)

(put 'erase-buffer 'disabled nil)
;; isearch(incremental search)
;; https://www.youtube.com/watch?v=9CdbfZXsrqg
(setq-default isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")
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

(use-package jinx
  :diminish
  ;; :hook
  ;; (text-mode . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))


(setq scheme-program-name "guile")

(defun my/org-narrow-forward ()
  "Move to the next subtree at same level, and narrow to it."
  (interactive)
  (widen)
  (forward-page 1)
  (narrow-to-page))
(global-set-key (kbd "C-x n f") #'my/org-narrow-forward)

(put 'narrow-to-page 'disabled nil)


(auto-insert-mode t)
(setq auto-insert-query nil)
(define-auto-insert "\\.org\\'"
  (lambda ()
    (unless (file-in-directory-p (buffer-file-name) "~/Documents/org/notes/") ;; 如果使用 denote-directory 来判断则会产生对 denote 加载的依赖
      (insert "#+TITLE: " (file-name-base (buffer-file-name)) "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n" )
      (newline)
      (goto-char (point-max)))))


;; set minibuffer
(setq enable-recursive-minibuffers t)
(setq minibuffer-depth-indicate-mode t)

;;custom treesitter
(setq treesit-extra-load-path `(,(expand-file-name "treesit" user-emacs-directory)))
(setq major-mode-remap-alist
      '(
	;; (yaml-mode . yaml-ts-mode) yaml-ts-mode does not have indent
	;; (html-mode . html-ts-mode) there is no html-ts-mode
	(bash-mode . bash-ts-mode)
	(js2-mode . js-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(python-mode . python-ts-mode)))


;; Display boot time message
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs initialized in %.2f seconds" (float-time (time-subtract after-init-time before-init-time)))))
