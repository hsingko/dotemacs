(setq package-enable-at-startup nil)
;; (setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-threshold 2147483648)
(setq gc-cons-threshold (* 1024 1024 50)
      gc-cons-percentage 0.6)

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
;; (setq package-quickstart t)

(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltip
(set-fringe-mode 8)
(menu-bar-mode -1)


;; 禁用系统输入法，参考：https://emacs-china.org/t/emacs/15948/9
(setq pgtk-use-im-context-on-new-connection nil)

(setq-default frame-title-format '("%b"))

;; (set-frame-parameter nil 'alpha-background 95)
;; (add-to-list 'default-frame-alist '(alpha-background . 95))

;;; not sure what it does
(setq frame-resize-pixelwise t)
