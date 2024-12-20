;; simplify the default emacs ui
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq font-lock-maximum-decoration t)
(setq cursor-type 'bar)

;; font family
(set-face-attribute 'default nil :family "SF Mono" :height 110 :weight 'regular)
(set-face-attribute 'variable-pitch nil :font (font-spec :family "Literata" :size 15  :weight 'regular))
(set-fontset-font t 'han (font-spec :family "IBM Plex Sans SC"))
(set-fontset-font t 'cjk-misc (font-spec :family "Noto Serif CJK SC"))

;;https://www.reddit.com/r/emacs/comments/wpr2n2/comment/ikj2vn1/?utm_source=share&utm_medium=web2x&context=3
(customize-set-variable 'org-blank-before-new-entry'((heading . nil)(plain-list-item . nil)))(setq org-cycle-separator-lines 1)

(column-number-mode)
(add-hook 'prog-mode-hook
		  (lambda ()
			(display-line-numbers-mode 1)))

(use-package diminish)

(diminish 'visual-line-mode)
(add-hook 'text-mode-hook
		  (lambda ()
			(visual-line-mode)))


(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars nil) ; adds extra indentation
  (org-modern-table nil)
  (org-modern-checkbox nil)
  (org-modern-tag nil)
  (org-modern-timestamp nil)
  (org-modern-block-name nil)
  :config
  (set-face-attribute 'org-modern-radio-target nil :height 120)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))


(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

(use-package olivetti
  :commands olivetti-mode
  :diminish
  :hook
  (txt-mode . olivetti-mode)
  (org-mode . olivetti-mode)
  (markdown-mode . olivetti-mode)
  :config
  (setq olivetti-style 'fancy)
  (add-hook 'olivetti-mode-on-hook #'(lambda ()
									   (text-scale-increase 1)
									   (setq-local line-spacing 0.2)))
  (add-hook 'olivetti-mode-off-hook #'(lambda ()
										(text-scale-decrease 1)
										(setq-local line-spacing nil)))
  (setq-default olivetti-body-width 60))


(use-package modus-themes
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-tinted))
  (setq modus-themes-italic-constructs t
		modus-themes-bold-constructs nil)
  (setq modus-themes-mixed-fonts nil)
  ;; (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-common-palette-overrides
		'((fg-heading-1 red-warmer)
		  (fg-heading-2 blue-warmer)))
  (setq modus-themes-prompts '(italic bold))
  (setq modus-themes-completions
		'((matches . (extrabold underline))
          (selection . (semibold))))
  (load-theme 'modus-operandi-deuteranopia :noconfirm))

(use-package ace-window
  :bind
  (("M-o" . ace-window))
  :config
  (add-to-list 'aw-ignored-buffers "*shell*"))

;; pixel scroll
(setq scroll-conservatively 100) ;; when next-line/previous-line move point out of screen, move by 1 line stead of scroll half screen and center the pointer
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defun +pixel-scroll-interpolate-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))
(defun +pixel-scroll-interpolate-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))
(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)


(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (help-mode . rainbow-mode)
  (emacs-lisp-mode . rainbow-mode))

(use-package spacious-padding
  :config
  (setq spacious-padding-subtle-mode-line t)
  (setq spacious-padding-widths
		'( :internal-border-width 5
           :header-line-width 4
           :mode-line-width 3
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))
  (spacious-padding-mode))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                       ("#+END_SRC" . "«")
                                       ("#+begin_src" . "»")
                                       ("#+end_src" . "«")
									   ("#+begin_comment" . "@")
									   ("#+end_comment" . "")
                                       ("#+end_src" . "")
									   ("#+begin_quote" . "“")
									   ("#+end_quote" . "”")))
(setq org-ellipsis " ⤵")

(add-hook 'org-mode-hook 'prettify-symbols-mode)


(setq display-buffer-alist '(
							 ("\\*Async Shell Command\\*"
							  (display-buffer-no-window))
							 ("\\*xeft\\*"
							  (display-buffer-at-bottom))))


;; 保证光标在跳转之后，完全显示当前行
(setq scroll-margin 1)

(use-package tab-bar
  :config
  (setq tab-bar-show 1)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints t)
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (keymap-global-set "s-w" #'tab-bar-close-tab)
  (keymap-global-set "s-t" #'tab-bar-new-tab)
  (keymap-global-set "s-{" #'tab-bar-switch-to-prev-tab)
  (keymap-global-set "s-}" #'tab-bar-switch-to-next-tab)
  )

(use-package mixed-pitch
  ;; :hook
  ;; (dired-mode . mixed-pitch-mode)
  ;; (org-mode . mixed-pitch-mode)
  ;; (markdown-mode . mixed-pitch-mode)
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-document-info-keyword)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-document-info)
  (setq mixed-pitch-set-height t))

;; (use-package valign
;;   :hook
;;   (org-mode . valign-mode))


;;; 默认的 ediff 会弹出一个新 frame ，非常别扭，这里采用了 prot 的配置
;;; Link: https://protesilaos.com/emacs/dotemacs#h:89edea05-4d94-4ea1-b2a8-5ad01422618c
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq-default tab-width 4)

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
(use-package all-the-icons-dired
  :diminish
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; 由于用了 mini-echo 所以需要这个包来区分 windows
;; mini-echo 的开发计划里也有这个功能，所以就期待它的更新吧
;; (use-package auto-dim-other-buffers
;;   :config
;;   (auto-dim-other-buffers-mode))

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode))
  :config
  (set-face-attribute 'markdown-pre-face nil :height 110))

(use-package pulsar
  :config
  (pulsar-global-mode))



(provide 'init-ui)
