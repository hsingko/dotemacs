
;; package management
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages") t)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))
(setq url-proxy-services
      '(("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))


(add-to-list 'exec-path "/home/rookie/.cargo/bin")
(add-to-list 'exec-path "/home/rookie/go/bin")
(add-to-list 'exec-path "/home/rookie/.local/bin/")
(add-to-list 'exec-path "/home/rookie/.npm-packages/bin/")

(setq use-package-enable-imenu-support t) ;; this line must placed before import `use-package`
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(setq use-short-answers t)

(setq custom-file "~/.emacs.d/custom.el")

;; change auto-save directory
;; I don't if it works
;; yes, it works
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosave/" t)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
;; (require 'init-meow)
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
(require 'init-shellcmd)
(require 'init-corfu)
(require 'init-eglot)
;; (require 'init-bridge)
(require 'init-magit)
(require 'init-undo)
(require 'init-phisearch)
(require 'denote-image)


(load "consult-buku.el")


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
    (unless (or (file-in-directory-p (buffer-file-name) "~/Documents/org/notes/")
				(file-in-directory-p (buffer-file-name) "~/Documents/org/silos/")) ;; 如果使用 denote-directory 来判断则会产生对 denote 加载的依赖
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
    (insert "Exec=" (file-truename (read-file-name "Program location: ")) "\n")
    (insert "Terminal=false\n")
    (insert "Type=Application\n")
    (insert "Icon=" (file-truename (read-file-name "Icon file location: ")) "\n")
    (insert "Categories=Utility")))

;; set minibuffer
(setq enable-recursive-minibuffers t)
(setq minibuffer-depth-indicate-mode t)

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

(use-package the-magical-str
  :after consult
  :load-path "~/Repo/the-magical-str.el/")

(defun consult-denote-pinyin ()
  (interactive)
  (find-file (consult--read (mapcar (lambda (fn)
									  (cons (get-magical-str (file-relative-name fn denote-directory))
											fn))
									(denote-directory-files))
							:lookup #'consult--lookup-cdr)))



;; substitute
(use-package substitute
  :bind
  (("C-c s" . #'substitute-prefix-map)
   :map substitute-prefix-map
   ("b" . #'substitute-target-in-buffer)
   ("d" . #'substitute-target-in-defun)
   ("r" . #'substitute-target-above-point)
   ("s" . #'substitute-target-below-point)))


(defun md/alfred ()
  (interactive)
  (get-buffer-create "*alfred*")
  (let ((frame (make-frame '((minibuffer . only)))))
    ;; Set frame parameters to make it as minimal as possible
    (modify-frame-parameters frame '((width . 80) (height . 20)))
    ;; Adjust frame position if needed
    (set-frame-position frame 200 200)
    (inhibit-message t)
    (hsk/consult-buku)
    (delete-frame frame)
    ;; If we don't kill the buffer it messes up future state.
    (kill-buffer "*alfred*")
    (other-window 1)))


(use-package zhau-abbrev
  :load-path "git/zhau-abbrev/"
  :custom
  (zhau-abbrev--pinyin-program "/home/rookie/go/bin/pinyin")
  :config
  (add-hook 'kill-emacs-hook #'zhau-abbrev-save-all-tables)
  ;; (add-hook 'org-mode-hook #'zhau-abbrev-mode)
  )

(use-package binky
  :bind
  (("M-k" . #'binky-binky))
  :config
  (binky-mode)
  (binky-margin-mode))


(setq garbage-collection-messages t)
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       'garbage-collect))



;; (setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq-default abbrev-mode 1)
(setq save-abbrevs nil) ;; 不提示保存 abbrev

;; generate linear range code
(use-package tiny
  :config
  (tiny-setup-default))


(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; 系统垃圾箱管理器，好用的
(use-package trashed)


(use-package casual-avy
  :bind (("M-p" . #'casual-avy-tmenu)))


(use-package orgmdb
  :config
  (setq orgmdb-omdb-apikey "48f9c561"))

(use-package expand-region
  :bind ("C-<tab>" . #'er/expand-region)
  :config
  (pending-delete-mode))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(use-package terminal-here
  :config
  (keymap-set global-map "C-<f5>" #'terminal-here-launch)
  (keymap-set global-map "C-<f6>" #'terminal-here-project-launch)
  (setq terminal-here-linux-terminal-command 'foot))


(use-package ox-hugo
  :after ox
  :config
  (setq org-hugo-default-static-subdirectory-for-externals "images"))


;; (use-package surround
;;   :config
;;   (keymap-set global-map "M-'" surround-keymap))

(use-package embrace
  :config
  (keymap-set global-map "M-'" #'embrace-commander))

(define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; test if this method is ok
