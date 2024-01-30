;;; init-bookmark.el --- bookmark                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  rookie

;; Author: rookie <rookie@onionhat>
;; Keywords: tools

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

(use-package bookmark+
  :load-path "git/bookmark-plus"
  :defer 2
  :config
  (setq bmkp-bmenu-state-file (expand-file-name ".emacs-bmk-bmenu-state.el"
						user-emacs-directory)))




(provide 'init-bookmark)
;;; init-bookmark.el ends here
