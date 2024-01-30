;;; init-lang.el --- language specified major mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <erokit@earth>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook
  (yaml-mode . display-line-numbers-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-wiki-links t)
  (setq markdown-wiki-link-search-type '(project)))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (define-key markdown-mode-map (kbd "C-c C-a r") #'markdown-footnote-return))

(use-package css-mode
  :ensure nil
  :mode ("\\.css\\'" . css-mode)
  :custom
  (css-indent-offset 2))

(use-package csv-mode)

;; disable html mode keybindings, it's useless, and conflicts with ace-window
(with-eval-after-load 'mhtml-mode
  (define-key mhtml-mode-map (kbd "M-o") nil))

(use-package systemd)

(provide 'init-lang)
;;; init-lang.el ends here
