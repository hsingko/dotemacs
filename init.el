; ui
;; simplify the default emacs ui
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltip
(set-fringe-mode 8)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(alpha-background . 95))
(pixel-scroll-precision-mode 1)
(recentf-mode 1)

;; font family
(set-face-attribute 'default nil :font (font-spec :family "Lekton" :size 18))
(set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji" :size 18))
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "LXGW WenKai Mono" :size 18 :weight 'regular))

; package management
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

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

;meow mode
(load-file "~/.config/emacs/meow.el")
(load-file "~/.config/emacs/comp.el")
(load-file "~/.config/emacs/consult.el")
(load-file "~/.config/emacs/marginalia.el")
(load-file "~/.config/emacs/embark.el")
(load-file "~/.config/emacs/chinese.el")
(load-file "~/.config/emacs/org.el")
(load-file "~/.config/emacs/denote.el")
(load-file "~/.config/emacs/corfu.el")
(load-file "~/.config/emacs/magit.el")
(load-file "~/.config/emacs/backup.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" default))
 '(package-selected-packages
   '(magit org-superstar markdown-mode all-the-icons-completion org-modern doom-modeline consult meow dracula-theme orderless vertico-posframe)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
