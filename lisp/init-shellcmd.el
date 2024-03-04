(use-package dwim-shell-command
  :ensure t
  :defer nil
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
     :utils "ffmpeg"))
  (defun my/dwim-shell-command-age-encrypt-file ()
    "Encrypt all marked files to *.age"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Encrypt with age"
     (concat
      "age -R " age-default-recipient
      " <<f>> > <<f>>.age")
     :utils "age"))
(defun my/dwim-shell-command-age-decrypt-file ()
    "Encrypt all marked files to *.age"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Decrypt file with age"
     (concat
      "age -d -i " age-default-identity
      " <<f>> > <<fne>>")
     :utils "age"))  
  )



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
  " 将当前目录下的所有压缩文件转换成 Kindle Comic Convert 便于处理的格式"
  (interactive)
  (let* ((series (read-string "Series Name:"))
	 (writer (read-string "Writer:"))
	 (dir (dired-current-directory)))
    (dolist (file (directory-files dir t))
      (let ((ext (file-name-extension file)))
	(when (and (file-regular-p file)
		   (gethash ext COMIC_SUPPORT_LIST nil))
	  (string-match "[0-9]+" (file-name-base file))
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
	   nil nil))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init-shellcmd)
;;; init-shellcmd.el ends here
