; ui
;; simplify the default emacs ui
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltip
(set-fringe-mode 8)
(menu-bar-mode -1)
(pixel-scroll-precision-mode 1)
(recentf-mode 1)
;port from nano-theme
(setq frame-title-format "Emacs")
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq font-lock-maximum-decoration t)

;; font family
(set-face-attribute 'default nil :font (font-spec :family "Iosevka Comfy Fixed" :size 17 :weight 'light))
;; (set-face-attribute 'default nil :font (font-spec :family "Lekton" :size 17 :weight 'light))
(set-face-attribute 'fixed-pitch nil :font (font-spec :family "Noto Serif CJK SC" :size 17 :weight 'light))
;; (set-face-attribute 'variable-pitch nil :font (font-spec :family "Noto Sans" :size 17 :weight 'light))
(set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji" :size 17))
(set-fontset-font t 'symbol (font-spec :family "Symbola" :size 17) nil 'prepend)
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "LXGW WenKai" :weight 'regular))
;; do not set chinese font size, use below code instead, see: https://baohaojun.github.io/perfect-emacs-chinese-font.html
(setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.1) ))

;; (setq resize-mini-windows nil)
;https://www.reddit.com/r/emacs/comments/wpr2n2/comment/ikj2vn1/?utm_source=share&utm_medium=web2x&context=3
(customize-set-variable 'org-blank-before-new-entry'((heading . nil)(plain-list-item . nil)))(setq org-cycle-separator-lines 1)

(column-number-mode)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode 1)))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  )

;; (setq modus-themes-links (quote (neutral-underline background)))
;; (setq modus-themes-mode-line '(borderless accented moody))
(setq modus-themes-region '(bg-only accented))
;; (setq modus-themes-completions 'opinionated)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-headings
      '(
	(1 . (1.1 light))
	(2 . (1.07 light))
	(3.  (1.05 light))
	(t . (light))))
(setq modus-themes-org-blocks 'tinted-background)
(load-theme 'modus-operandi-deuteranopia)
(defun +custom-modeline ()
  (set-face-attribute 'mode-line nil
		      :box nil
		      :overline (face-attribute 'default :foreground)
		      :background (face-attribute 'default :background)
		      :font "UbuntuMono Nerd Font Mono-12"
		      )
  (set-face-attribute 'mode-line-inactive nil
		      :box nil
		      :overline nil
		      :background (face-attribute 'default :background)
		      :font "UbuntuMono Nerd Font Mono-12"
		      )
  )
(setq modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia))

(+custom-modeline)
(add-hook 'modus-themes-after-load-theme-hook #'+custom-modeline)
(vertico-posframe-mode 1)
(setq vertico-posframe-border-width 1)

(use-package diminish)
(diminish 'visual-line-mode)

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars nil) ; adds extra indentation
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

(use-package olivetti
  :commands olivetti-mode
  :diminish
  ;; :hook
  ;; (org-mode . olivetti-mode)
  :config
  (add-hook 'olivetti-mode-on-hook #'(lambda ()
				    (text-scale-increase 1)))
  (add-hook 'olivetti-mode-off-hook #'(lambda ()
				    (text-scale-decrease 1)))
  (setq-default olivetti-body-width 0.39)
  ;; (setq-default olivetti-minimum-body-width 78)
  )

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package org-modern-indent
  :load-path "git/org-modern-indent"
  :hook
  (org-indent-mode . org-modern-indent-mode))

(set-frame-parameter nil 'alpha '(95 . 100))
