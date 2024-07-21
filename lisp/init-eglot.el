;;; init-eglot.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  rookie

;; Author: rookie <rookie@onionhat>
;; Keywords:

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
(use-package eglot
  :ensure nil
  :defer t
  :hook
  (python-ts-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (mhtml-mode . eglot-ensure))

;; (use-package eldoc-box
;;   :hook
;;   (eglot-managed-mode . eldoc-box-hover-mode))

(use-package eglot-booster
  :load-path "git/eglot-booster"
  :after eglot
  :config	(eglot-booster-mode))

(provide 'init-eglot)
;;; init-eglot.el ends here
