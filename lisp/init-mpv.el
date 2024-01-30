;;; init-mpv.el --- use mpv in emacs                 -*- lexical-binding: t; -*-

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

(use-package empv
  :config
  (bind-key "C-x m" empv-map)
  (add-to-list 'empv-video-file-extensions "webm"))



(provide 'init-mpv)
;;; init-mpv.el ends here
