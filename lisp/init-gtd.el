;;; init-gtd.el --- org-gtd configuration            -*- lexical-binding: t; -*-

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
(use-package org-gtd
  :after org
  :demand t
  :init
  (setq org-gtd-update-ack "3.0.0")
  (setq org-gtd-directory "~/Documents/org/gtd/")
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   :map org-gtd-clarify-map
   ("C-c c" . org-gtd-organize)))


(provide 'init-gtd)
;;; init-gtd.el ends here
