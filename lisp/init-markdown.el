(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-wiki-links t)
  (setq markdown-wiki-link-search-type '(project)))

;; (use-package obsidian
;;   :ensure t
;;   :demand t
;;   :config
;;   (obsidian-specify-path "~/Documents/obsidian")
;;   (global-obsidian-mode t)
;;   :custom
;;   ;; This directory will be used for `obsidian-capture' if set.
;;   (obsidian-inbox-directory "+Cards")
;;   :bind (:map obsidian-mode-map
;;   ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
;;   ("C-c C-o" . obsidian-follow-link-at-point)
;;   ;; Jump to backlinks
;;   ("C-c C-b" . obsidian-backlink-jump)
;;   ;; If you prefer you can use `obsidian-insert-link'
;;   ("C-c C-l" . obsidian-insert-wikilink)))

(provide 'init-markdown)
