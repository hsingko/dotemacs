
; package management
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)


					;meow mode
(load-file "~/.config/emacs/ui.el")
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
(load-file "~/.config/emacs/lang.el")
(load-file "~/.config/emacs/tools.el")
(load-file "~/.config/emacs/general.el")
(load-file "~/.config/emacs/elfeed.el")
(load-file "~/.config/emacs/journal.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" default))
 '(package-selected-packages
   '(org-journal general which-key yaml-mode magit org-superstar markdown-mode all-the-icons-completion org-modern doom-modeline consult meow dracula-theme orderless vertico-posframe)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
