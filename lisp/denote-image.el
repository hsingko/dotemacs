;;; denote-image.el --- denote image implement       -*- lexical-binding: t; -*-

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

(defcustom deimg-dir "~/Documents/org/images/"
  "default denote image directory")

(defcustom deimg-system-shot-dir "~/Pictures/Screenshots/"
  "the location of system scrrenshots")

(defun deimg--get-latest-file (path)
  "get latest file"
  (cl-remove-if
   (lambda (x) (member x '("." "..")))
   (car (directory-files path 'full nil #'file-newer-than-file-p))))

(defun deimg--get-current-dir ()
  "get current image directory for this note"
  (file-name-as-directory
   (expand-file-name
    (denote-retrieve-filename-identifier
     (buffer-file-name))
    deimg-dir)))

(defun deimg-display ()
  "display associate images for current denote buffer, using image-dired"
  (interactive)
  (image-dired (deimg--get-current-dir)))

(defun deimg-move-latest-shot ()
  "move latest screenshot to current denote associated dir"
  (interactive)
  (let ((tar-dir (deimg--get-current-dir)))
    (unless (file-exists-p tar-dir)
      (make-directory tar-dir))
    (rename-file
     (deimg--get-latest-file deimg-system-shot-dir)
     (deimg--get-current-dir))))

(provide 'denote-image)
;;; denote-image.el ends here
