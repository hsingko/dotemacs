;;; init-abbrev.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <erokit@earth>
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
(defun my-abbrev-no-insert ()
  t)

(put 'my-abbrev-no-insert 'no-self-insert t)

(define-abbrev-table 'text-mode-abbrev-table
  '(
    ("1jj" "京极夏彦" my-abbrev-no-insert)
    ("1sjt" "三津田信三" my-abbrev-no-insert)
    ("1xzby" "西泽保彦" my-abbrev-no-insert)))

(add-hook 'text-mode-hook 'abbrev-mode)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(provide 'init-abbrev)
;;; init-abbrev.el ends here
