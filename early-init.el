(setq package-enable-at-startup nil)
;; (setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold 2147483648)
(setq gc-cons-threshold (* 1024 1024 50)
      gc-cons-percentage 0.6)

;;(setq gc-cons-threshold most-positive-fixnum)

;; (add-hook 'emacs-startup-hook (lambda ()
;;                                 (setq gc-cons-threshold (* 16 1024 1024))))



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

(add-hook 'window-size-change-functions 'frame-hide-title-bar-when-maximized)


;;;
;;
;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 128 1024))  ; 128kb

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)
