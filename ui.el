; ui
;; simplify the default emacs ui
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltip
(set-fringe-mode 8)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(pixel-scroll-precision-mode 1)
(recentf-mode 1)

;; font family
(set-face-attribute 'default nil :font (font-spec :family "Lekton" :size 18))
(set-fontset-font t 'symbol (font-spec :family "Symbola" :size 18) nil 'prepend)
(set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji" :size 18))
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "LXGW WenKai Mono" :size 18 :weight 'regular))
; https://www.reddit.com/r/emacs/comments/wpr2n2/comment/ikj2vn1/?utm_source=share&utm_medium=web2x&context=3
(customize-set-variable 'org-blank-before-new-entry'((heading . nil)(plain-list-item . nil)))(setq org-cycle-separator-lines 1)

;; theme
(setq modus-themes-mode-line '(borderless accented))
(setq modus-themes-region '(bg-only accented))
;; (setq modus-themes-completions 'opinionated)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-headings
      '(
        ;; (1 . (rainbow))
        ;; (2 . (rainbow))
        ;; (3 . (rainbow))
        (t . (rainbow light)))
      )
;; (setq modus-themes-scale-headings t)
(setq modus-themes-org-blocks 'tinted-background)
(load-theme 'modus-operandi t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package markdown-mode)
