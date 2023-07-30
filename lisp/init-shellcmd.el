;;; init-shellcmd.el --- init for shell commands provide by dwim-shell-command  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <erokit@earth>
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
(use-package dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands)
  (setq dwim-shell-command-default-command "")
  (defun my/dwim-shell-command-convert-to-gif ()
    "Convert all marked videos to optimized gif(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to gif"
     "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
     :utils "ffmpeg")))

;; comic generator
(defcustom COMIC_META `
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<ComicInfo xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
 xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
  <Series>%s</Series>
  <Number>%s</Number>
  <Writer>%s</Writer>
</ComicInfo>
"
  "default comic meta xml format")

(defcustom COMIC_SUPPORT_LIST #s(hash-table
				 :size 3
				 test equal
				 data (
				       "7z" "cb7"
				       "rar" "cbr"
				       "zip" "cbz"))
  "kindle comic converter support list")

(defun my/generate-comic-meta-in-current-directory ()
  (interactive)
  (let* ((series (read-string "Series Name:"))
	 (writer (read-string "Writer:"))
	 (dir (dired-current-directory)))
    (dolist (file (directory-files dir t))
      (let ((ext (file-name-extension file)))
	(when (and (file-regular-p file)
		   (gethash ext COMIC_SUPPORT_LIST nil)
		   (string-match "[0-9]+" (file-name-base file)))
	  
	  (async-shell-command
	   (format "echo '%s'|7z a -siComicInfo.xml \"%s\" && mv \"%s\" \"%s\""
		   (format COMIC_META
			   series
			   (string-to-number (match-string 0 (file-name-base file)))
			   writer)
		   file
		   file
		   (concat
		    (file-name-directory file)
		    (file-name-base file)
			   "."
			   (gethash ext COMIC_SUPPORT_LIST)))
	   nil nil)
	  )))))
(put 'narrow-to-region 'disabled nil)

(provide 'init-shellcmd)
;;; init-shellcmd.el ends here
