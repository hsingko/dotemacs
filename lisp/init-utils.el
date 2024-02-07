(defun +find-file-in-config()
  (interactive)
  (cd "~/.emacs.d")
  (call-interactively #'find-file-in-project))

(defun +delete-this-file()
  "kill this file and buffer with no confirm"
  (interactive)
  (delete-file (buffer-file-name))
  (kill-this-buffer))


(defun doom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (doom-files--update-refs old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (message "File moved to %S" (abbreviate-file-name new-path))))


(defun doom--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))


(defun doom/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (doom--sudo-file-path file)))

(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (let ((p (point)))
    (find-file
     (doom--sudo-file-path
      (or buffer-file-name
          (when (or (derived-mode-p 'dired-mode)
                    (derived-mode-p 'wdired-mode))
            default-directory))))
    (goto-char p)
    (kill-buffer (other-buffer (current-buffer) 1))))

(defun doom/sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (doom--sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))


(defun org-quote-region ()
  "Convert each line in the selected region to a quote block in Org mode."
  (interactive)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char end)
          (insert "\n#+END_QUOTE\n")
          (goto-char start)
          (insert "#+BEGIN_QUOTE\n")
          (forward-line)
          (while (<= (point) end)
            (insert "#+BEGIN_QUOTE ")
            (end-of-line)
            (insert "\n#+END_QUOTE\n")
            (forward-line))))
    (message "No region selected")))


(use-package elec-pair
  :ensure nil
  :hook
  (text-mode . electric-pair-mode))


(defun ar/remove-from-list-variable ()
  (interactive)
  (let* ((var (intern
               (completing-read "From variable: "
                                (let (symbols)
                                  (mapatoms
                                   (lambda (sym)
                                     (when (and (boundp sym)
                                                (seqp (symbol-value sym)))
                                       (push sym symbols))))
                                  symbols) nil t)))
         (values (mapcar (lambda (item)
                           (setq item (prin1-to-string item))
                           (concat (truncate-string-to-width
                                    (nth 0 (split-string item "\n"))
                                    (window-body-width))
                                   (propertize item 'invisible t)))
                         (symbol-value var)))
         (index (progn
                  (when (seq-empty-p values) (error "Already empty"))
                  (seq-position values (completing-read "Delete: " values nil t)))))
    (unless index (error "Eeek. Something's up."))
    (set var (append (seq-take (symbol-value var) index)
                     (seq-drop (symbol-value var) (1+ index))))
    (message "Deleted: %s" (truncate-string-to-width
                            (seq-elt values index)
                            (- (window-body-width) 9)))))


(require 'subr-x)



(defcustom DESKTOP_ENTRY_TEMPLATE
  "[Desktop Entry]\nName=%s\nExec=%s\nType=Application\nCategories=Utility;\n"
  " desktop entry template in ~/.local/share/applications")

;; 未测试
(defun my/install-appimage ()
  " 将指定的 Appimage 文件自动拷贝到 Applications 目录并生成 desktop 文件"
  (interactive)
  (let* ((filepath (read-file-name "Select AppImage:" nil nil nil nil "Appimage"))
	 (name (read-string "Set the application name:" nil (file-name-base filepath)))
	 (new-name (concat name ".Appimage"))
	 (new-path (expand-file-name new-name "~/Applications")))
    ;; make Appimage executable
    (set-file-modes filepath
		    (file-modes-symbolic-to-number "+x"
						   (file-modes filepath)))
    ;; rename and move file
    (rename-file filepath new-path)
    ;; create desktop entry in ~/.local/share/applications
    (with-temp-file (expand-file-name
		     (concat name ".desktop")
		     "~/.local/share/applications")
      (insert (format DESKTOP_ENTRY_TEMPLATE
		      name
		      new-path)))
    (async-shell-command "update-desktop-database")
    (message "Appimage installation success!")))


(defun +insert-time-string-in-ISO8601 ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z")))


(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))


(defun +consult-keydo-on-region ()
  (interactive)
  (funcall-interactively 'consult-ripgrep
			 (expand-file-name "rime" user-emacs-directory)
			 (buffer-substring (region-beginning)
					   (region-end))))


(defun hsk/consult-imenu-respect-narrow ()
  "after consult-imenu, keep org narrowing status"
  (interactive)
  (if (org-buffer-narrowed-p)
      (progn (consult-imenu)
	     (org-narrow-to-subtree))
    (consult-imenu)))


;; steal from prot
(defun hsk/make-frame-floating-with-current-buffer ()
  (interactive)
  (make-frame '((name . "floating")
		(window-system . x)
		(minibuffer . nil))))




;;; consult-buku

;;; consult font family
(defun hsk/insert-font-family ()
  "consult and insert font family name"
  (interactive)
  (insert (consult--read (font-family-list))))



;;; consult-denote
(defun consult-denote-file-prompt ()
  (expand-file-name
   (consult--read (mapcar (lambda (f) (file-relative-name f denote-directory))
			  (denote-directory-files)))
   denote-directory))

(defun consult-denote ()
  (interactive)
  (find-file (consult-denote-file-prompt)))

(defun consult-denote-link (file file-type description &optional id-only)
  (interactive
   (let ((file (consult-denote-file-prompt)) ;; <====== Hey, I'm here
         (type (denote-filetype-heuristics (buffer-file-name))))
     (list
      file
      type
      (denote--link-get-description file type)
      current-prefix-arg)))
  (let* ((beg (point))
         (identifier-only (or id-only (string-empty-p description))))
    (insert
     (denote-format-link
      file
      (denote-link--file-type-format file-type identifier-only)
      description))
    (unless (derived-mode-p 'org-mode)
      (make-button beg (point) 'type 'denote-link-button))))


(defun consult-hugo-blog ()
  (interactive)
  (let (files)
    (setq files (directory-files-recursively "~/Documents/Blog/content"  "\\.\\(org\\|md\\)$"))
    (find-file (consult--read (mapcar (lambda (f)
					(cons
					 (consult-hugo-retrieve-title-value f (intern (file-name-extension f))) f))
				      files)
			      :lookup #'consult--lookup-cdr))))


(defun consult-hugo-retrieve-title-value (f ext)
  (if (eq ext 'md)
      (denote-retrieve-title-value f 'markdown-yaml)
    (denote-retrieve-title-value f 'org)))


(setq abbrev_directory "~/.emacs.d/abbrev/")
(add-to-list 'load-path abbrev_directory)

(defun consult-load-abbrevs ()
  (interactive)
  (load (consult--read (directory-files abbrev_directory))))


(defun hsk/convert-region-t2y (start end)
  "convert region from toml to yaml, using external command jyt"
  (interactive "r")
  (shell-command-on-region start end "jyt ty" nil t))

(defun hsk/convert-region-y2t (start end)
  "convert region from yaml to toml, using external command jyt"
  (interactive "r")
  (shell-command-on-region start end "jyt yt" nil t))

(defcustom renpy_games_location "~/Downloads"
  "where did you put those games?")

(defun hsk/find-sh-scripts-recursively (directory)
  "Find all .sh script files under DIRECTORY recursively."
  (interactive "DDirectory: ")
  (let ((sh-scripts '()))
    (dolist (file (directory-files-recursively directory "\\.sh$"))
      (when (file-regular-p file)
        (push file sh-scripts)))
    sh-scripts))

(defun hsk/run-renpy-games-in-download ()
  "select and run renpy game from ~/Downloads directory"
  (interactive)
  (async-shell-command
   (consult--read (hskf/ind-sh-scripts-recursively renpy_games_location))))

(provide 'init-utils)
