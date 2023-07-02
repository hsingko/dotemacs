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
;; (require 'init-dashboard)
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
(require 'init-html)
(require 'init-formater)
(require 'init-abbrev)
(require 'init-shellcmd)

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
;; but I still don't know how to use mark
;; (global-unset-key (kbd "C-@"))
;; (global-set-key (kbd "M-SPC") 'set-mark-command)


(use-package jinx
  :diminish
  :hook
  (text-mode . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(unbind-key (kbd "C-c C-k") org-mode-map)

(setq scheme-program-name "guile")

(defun my/org-narrow-forward ()
  "Move to the next subtree at same level, and narrow to it."
  (interactive)
  (widen)
  (forward-page 1)
  (narrow-to-page))
(global-set-key (kbd "C-x n f") #'my/org-narrow-forward)

;;; hugo setup
(defcustom HUGO_DIRECTORY "~/Documents/Blog"
  "The default hugo blog directory")
(defun extract-double-quoted-substring (string)
  "Extracts the double-quoted substring from the given string."
  (when (string-match "\"\\([^\"]*\\)\"" string)
    (match-string 1 string)))
(defun create-hugo-post ()
  (interactive)
  (let* ((default-directory HUGO_DIRECTORY)
	 (title (replace-regexp-in-string
		 "\\s-"
		 "-"
		 (read-string "Blog Title:")))
	 (section (read-string "Section:"))
	 (date (decode-time (current-time)))
	 (year (nth 5 date))
	 (month (nth 4 date))
	 (day (nth 3 date))
	 (res (shell-command-to-string
	       (format
		"hugo new %s/%s/%s/%s/%s/index.md" section year month day title)))
	 (path (extract-double-quoted-substring res)))
    (message res)
    (org-insert-link nil path title)))


(put 'narrow-to-page 'disabled nil)


(auto-insert-mode t)

(define-auto-insert "\\.org\\'" ; Match .org extension
  '(nil ;; No condition, always match
    "#+TITLE: " (file-name-base (buffer-file-name)) \n ; Insert title
    "#+DATE: " (format-time-string "%Y-%m-%d") \n ; Insert date
    \n ; Insert a blank line
    _)) ; Place the cursor here

;; (ffap-bindings)

;;; minibuffer
(setq enable-recursive-minibuffers t)
(setq minibuffer-depth-indicate-mode t)



;;; treesit
(setq treesit-extra-load-path `(,(expand-file-name "treesit" user-emacs-directory)))
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
	(html-mode . html-ts-mode)
	(bash-mode . bash-ts-mode)
	(js2-mode . js-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(python-mode . python-ts-mode)))
