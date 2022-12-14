;;; completion.el -*- lexical-binding: t; -*-

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first nil)
  (corfu-echo-mode t)
  ;; (corfu-popupinfo-mode t)
  (corfu-popupinfo-delay 0.5)
  :hook
  (
   (emacs-lisp-mode . (lambda ()
			(corfu-mode)
			(corfu-popupinfo-mode)
			))
   (python-mode . (lambda ()
		    (corfu-mode)
                    (corfu-popupinfo-mode)
                    ))

   )
  ;; :init
  ;; (global-corfu-mode)
  :bind (:map corfu-map
              ("<escape>" . corfu-quit)
              ([tab] . corfu-next))
  )

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package cape
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  )


(use-package eglot
  :ensure nil
  :hook
  (python-mode . eglot-ensure))

(use-package eldoc
  :after eglot
  :diminish
  :config
  (use-package eldoc-box
  :after eldoc
  :init
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))
  )
