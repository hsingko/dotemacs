
; package management
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

(load-file "~/.config/emacs/ui.el")
(load-file "~/.config/emacs/meow.el")
(load-file "~/.config/emacs/keymap.el")
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
(load-file "~/.config/emacs/lang.el")
(load-file "~/.config/emacs/tools.el")
(load-file "~/.config/emacs/journal.el")
(load-file "~/.config/emacs/yasnippet.el")
(load-file "~/.config/emacs/hugo.el")
(load-file "~/.config/emacs/undo.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-modern-indent helpful ox-hugo yasnippet-snippets yasnippet ace-window org-journal which-key yaml-mode magit org-superstar all-the-icons-completion org-modern doom-modeline meow orderless vertico-posframe)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
