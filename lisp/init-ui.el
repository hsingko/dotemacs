;; simplify the default emacs ui
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltip
(set-fringe-mode 8)
(menu-bar-mode -1)
					;(pixel-scroll-precision-mode 1)
					;port from nano-theme
					;(setq frame-title-format nil)
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq font-lock-maximum-decoration t)
;; (setq default-frame-alist '((undecorated . t))) ;;; 隐藏窗口标题栏
;; font family
;; (set-face-attribute 'default nil :font (font-spec :family "FiraCode Nerd Font Mono" :size 17 :weight 'thin))
(set-face-attribute 'default nil :font (font-spec :family "IntelOne Mono" :size 16 :weight 'regular))
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Sarasa Mono CL" :weight 'regular))

;; (setq resize-mini-windows nil)
;;https://www.reddit.com/r/emacs/comments/wpr2n2/comment/ikj2vn1/?utm_source=share&utm_medium=web2x&context=3
(customize-set-variable 'org-blank-before-new-entry'((heading . nil)(plain-list-item . nil)))(setq org-cycle-separator-lines 1)

(column-number-mode)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode 1)))


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
  (setq-default olivetti-body-width 0.4)
  ;; (setq-default olivetti-minimum-body-width 78)
  )


(set-frame-parameter nil 'alpha '(95 . 100))

(use-package ef-themes
  :config
  (setq ef-themes-headings
	(quote ((t . (regular))))))


(load-theme 'ef-cyprus t)

;; window
(winner-mode)

(use-package ace-window
  :bind
  (("M-o" . ace-window)))

(pixel-scroll-precision-mode 1)

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

(provide 'init-ui)
