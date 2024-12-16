(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width)
  (corfu-scroll-margin 4)
  (corfu-auto-prefix 2)
  :bind
  (:map corfu-map
	([escape] . corfu-quit))
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (corfu-history-mode)
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  :init
  (add-hook 'prog-mode-hook #'corfu-mode)
  (add-hook 'inferior-python-mode-hook #'corfu-mode))

(use-package cape
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

;; (use-package nerd-icons-corfu
;;   :after corfu
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(provide 'init-corfu)
