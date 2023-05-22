; package management
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq custom-file "custom.el")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package markdown-mode)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
(require 'init-meow)
(require 'init-consult)
(require 'init-vertico)
(require 'init-marginalia)
(require 'init-utils)
(require 'init-org)
(require 'keymaps)
(require 'init-cn)
(require 'init-calibre)
(require 'init-find)
(require 'init-yas)
(require 'init-bridge)
(require 'init-dired)
(require 'init-rg)
(require 'init-embark)
(require 'init-save)
(require 'init-magit)


