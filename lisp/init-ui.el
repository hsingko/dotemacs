;; simplify the default emacs ui
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltip
(set-fringe-mode 8)
(menu-bar-mode -1)
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq font-lock-maximum-decoration t)
(setq cursor-type 'bar)

;; font family
(set-face-attribute 'default nil :font (font-spec :family "JetBrains Mono"  :size 16 :weight 'light))
(set-face-attribute 'variable-pitch nil :font (font-spec :family "Charis SIL" :size 16  :weight 'regular))
(set-fontset-font t 'han (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'cjk-misc (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'kana "Noto Sans CJK JP")


;; (setq resize-mini-windows nil)
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
  (org-modern-tag t)
  (org-modern-timestamp t)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))


(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

(use-package olivetti
  :commands olivetti-mode
  :diminish
  :hook
  (org-mode . olivetti-mode)
  :config
  (add-hook 'olivetti-mode-on-hook #'(lambda ()
				       (text-scale-increase 1)))
  (add-hook 'olivetti-mode-off-hook #'(lambda ()
					(text-scale-decrease 1)))
  (setq-default olivetti-body-width 85))

(use-package ef-themes
  :config
  (setq ef-themes-mixed-fonts t)
  (setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 variable-pitch light 1.7)
        (1 variable-pitch light 1.6)
        (2 variable-pitch regular 1.4)
        (3 variable-pitch regular 1.3)
        (4 variable-pitch regular 1.2)
        (5 variable-pitch 1.1) ; absence of weight means `bold'
        (6 variable-pitch 1.1)
        (7 variable-pitch 1.1)
        (t variable-pitch 1.1)))
  (load-theme 'ef-tritanopia-light t))


(use-package ace-window
  :bind
  (("M-o" . ace-window)))

;; (setq default-frame-alist (append default-frame-alist '((alpha-background . 93))))

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

(use-package feline
  :config (feline-mode)
  :custom
  (feline-line-prefix "🎯")
  (feline-column-prefix ":")
  (feline-mode-symbols
   '(emacs-lisp-mode "🤖"
		     python-ts-mode "🐍"
		     org-mode "🐮")))



(use-package spacious-padding
  :config
  (setq spacious-padding-subtle-mode-line t)
  (setq spacious-padding-widths
	'( :internal-border-width 2
           :header-line-width 4
           :mode-line-width 3
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))
  (spacious-padding-mode))


(use-package mixed-pitch
  :config
  ;; (add-hook 'org-mode-hook 'mixed-pitch-mode)
  )

(provide 'init-ui)
