; package management
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(setq custom-file "~/.config/emacs/custom.el")


(load-file "~/.config/emacs/ui.el")
(load-file "~/.config/emacs/backup.el")
(load-file "~/.config/emacs/keymap.el")
(load-file "~/.config/emacs/undo.el")
(load-file "~/.config/emacs/meow.el")
(load-file "~/.config/emacs/comp.el")
(load-file "~/.config/emacs/consult.el")
(load-file "~/.config/emacs/marginalia.el")
(load-file "~/.config/emacs/embark.el")
(load-file "~/.config/emacs/chinese.el")
(load-file "~/.config/emacs/org.el")
(load-file "~/.config/emacs/denote.el")
(load-file "~/.config/emacs/consult-denote.el")
(load-file "~/.config/emacs/corfu.el")
(load-file "~/.config/emacs/tools.el")
;(load-file "~/.config/emacs/hugo.el")
;(load-file "~/.config/emacs/nov.el")
