;; reference: https://github.com/condy0919/emacs-newbie/blob/master/introduction-to-builtin-modes.md

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 500)
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude `("/ssh:"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))

(use-package saveplace
  :ensure nil
  :hook
  (after-init . save-place-mode))


;; 优化长行下 emacs 的表现
(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))


;; 当文件被其它应用更新后，自动重新加载
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package simple
  :ensure nil
  :hook
  (after-init . size-indication-mode) ;; word count
  )

(provide 'init-builtin)
