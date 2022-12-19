; ui
;; simplify the default emacs ui
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltip
(set-fringe-mode 8)
(menu-bar-mode -1)
;; (add-to-list 'default-frame-alist '(alpha-background . 95))
(pixel-scroll-precision-mode 1)
(recentf-mode 1)
;port from nano-theme
(setq frame-title-format nil)
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq font-lock-maximum-decoration t)
;; (setq cursor-type 'bar)
;; (setq cursor-type '(bar . 4)) 
;; (setq line-spacing 0)
;; font family
(set-face-attribute 'default nil :font (font-spec :family "Iosevka Comfy Fixed" :size 18 :weight 'light))
(set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji" :size 18))
(set-fontset-font t 'symbol (font-spec :family "Symbola" :size 18) nil 'prepend)
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "LXGW WenKai Mono" :size 18 :weight 'regular))

;; (setq resize-mini-windows nil)
;https://www.reddit.com/r/emacs/comments/wpr2n2/comment/ikj2vn1/?utm_source=share&utm_medium=web2x&context=3
(customize-set-variable 'org-blank-before-new-entry'((heading . nil)(plain-list-item . nil)))(setq org-cycle-separator-lines 1)

(column-number-mode)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode 1)))

(use-package telephone-line
  :custom
  (telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-height 24)
  (telephone-line-evil-use-short-tag t)  
  :config
  (setq telephone-line-lhs
	'((evil   . (telephone-line-meow-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (
		     ;; telephone-line-minor-mode-segment
                     telephone-line-buffer-segment
		     ))))
  (setq telephone-line-rhs
	'((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
	telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
	telephone-line-primary-right-separator 'telephone-line-cubed-right
	telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
	telephone-line-evil-use-short-tag t)
  (telephone-line-mode)
  )



(use-package markdown-mode)
(use-package writeroom-mode)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-gruvbox-light t)
;;   (setq doom-gruvbox-light-variant "medium"
;; 	doom-gruvbox-light-padded-modeline t
;; 	doom-gruvbox-light-brighter-modeline t
;; 	)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(setq modus-themes-links (quote (neutral-underline background)))
(setq modus-themes-mode-line '(borderless accented))
(setq modus-themes-region '(bg-only accented))
;; (setq modus-themes-completions 'opinionated)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-headings
      '((t . (rainbow light))))
(setq modus-themes-org-blocks 'tinted-background)
(load-theme 'modus-operandi)

