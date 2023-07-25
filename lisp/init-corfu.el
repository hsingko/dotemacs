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
  :hook
  (prog-mode . corfu-mode))

(use-package cape
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'init-corfu)
