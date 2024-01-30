;; package management
;; (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages")
;;                          ("melpa" . "https://melpa.org/packages")))
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

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
      '((".*" "~/.emacs.d/autosave/" t)))
(setq create-lockfiles nil)

;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
(require 'init-meow)
(require 'init-consult)
(require 'init-vertico)
(require 'init-marginalia)
(require 'init-utils)
(require 'init-lang)
(require 'init-encrypt)
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
(require 'init-xeft)
(require 'init-html)
(require 'init-formater)
(require 'init-abbrev)
(require 'init-shellcmd)
(require 'init-corfu)
(require 'init-magit)
(require 'init-undo)
(require 'init-mpv)
(require 'init-phisearch)
(require 'init-bookmark)
(require 'denote-image)

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

(use-package server
  :config
  (unless (server-running-p)
  (server-start)))



;; marks
(setq-default set-mark-command-repeat-pop t)

;; (use-package jinx
;;   :diminish
;;   :hook
;;   (text-mode . jinx-mode)
;;   :config
;;   (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(use-package ispell
  :config
  (setq ispell-program-name "/usr/bin/hunspell"))


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
      (insert "#+title: " (file-name-base (buffer-file-name)) "\n")
      (insert "#+date: " (format-time-string "%Y-%m-%dT%H:%M:%S%z") "\n" )
      (newline)
      (goto-char (point-max)))))
(define-auto-insert "\\.desktop\\'"
  (lambda ()
    (insert "[Desktop Entry]\n")
    (insert "Name=" (read-from-minibuffer "Entry name: ") "\n")
    (insert "GenericName=" (read-from-minibuffer "GenericName: ") "\n")
    (insert "Comment=\n")
    (insert "Exec=" (read-file-name "Program location: ") "\n")
    (insert "Terminal=false\n")
    (insert "Type=Application\n")
    (insert "Icon=" (read-file-name "Icon file location: ") "\n")
    (insert "Categories=Utility")))


;; set minibuffer
(setq enable-recursive-minibuffers t)
(setq minibuffer-depth-indicate-mode t)

;;custom treesitter
;; (setq treesit-extra-load-path `(,(expand-file-name "treesit" user-emacs-directory)))
;; (setq major-mode-remap-alist
;;       '(
;; 	;; (yaml-mode . yaml-ts-mode) yaml-ts-mode does not have indent
;; 	(bash-mode . bash-ts-mode)
;; 	(js2-mode . js-ts-mode)
;; 	(typescript-mode . typescript-ts-mode)
;; 	(json-mode . json-ts-mode)
;; 	(css-mode . css-ts-mode)
;; 	(python-mode . python-ts-mode)))


;; Display boot time message
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs initialized in %.2f seconds" (float-time (time-subtract after-init-time before-init-time)))))


;;; savehist
(use-package savehist
  :ensure nil
  :init
  (savehist-mode)
  :config
  (setq command-history t))

(setq enable-local-variables :safe)


;; suppress native comp warnings
(setq native-comp-async-report-warnings-errors nil)

;; substitute
(use-package substitute)
