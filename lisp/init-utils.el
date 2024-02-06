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

(defun get--pinyin-first-letters (string)
  "translate Chinese Words to pinyin using external command: pypinyin"
  (string-trim-right
   (shell-command-to-string (format "~/.local/bin/pypinyin -f slug -p '' -s FIRST_LETTER %s" string))))

(defun get--org-headlines-under-tree (pattern)
  "获取给定 headline 下所有子节点字符串"
  (save-excursion
    (org-with-point-at (org-find-exact-headline-in-buffer pattern)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (org-element-property :title headline))))))



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


(defun my/insert-font-name ()
  "Insert the selected font name as a quoted string."
  (interactive)
  (let ((font (completing-read "Select font: " (font-family-list))))
    (when (not (string-empty-p font))
      (insert (format "\"%s\"" font)))))




(define-minor-mode textnov-mode
  "text novel minor mode"
  :lighter "Nov"
  :init-value nil
  (if textnov-mode
      (progn
	(setq imenu-generic-expression '((nil "^\\(　+第[一二三四五六七八九十零]+章.*\\)" 1)))
	(read-only-mode)
	(setq imenu-sort-function 'imenu--sort-by-position))
    (progn
      (read-only-mode -1)
      (message "textnov-mode disabled"))))


(defun +insert-time-string-in-ISO8601 ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

(defun xah-html-escape-char-to-entity (@begin @end &optional @entity-to-char-p)
  "Replace HTML chars & < > to HTML entities on current text block or selection.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

Highlight changed places.
If `universal-argument' is called first, the replacement direction is reversed.

When called in lisp code, @begin @end are region begin/end positions. If @entity-to-char-p is true, reverse change direction.

URL `http://xahlee.info/emacs/emacs/elisp_replace_html_entities_command.html'
Version 2020-08-30"
  (interactive
   (save-excursion
     (list
      (if (use-region-p)
          (region-beginning)
        (progn
          (re-search-backward "\n[ \t]*\n" nil "move")
          (re-search-forward "\n[ \t]*\n" nil "move")
          (point)))
      (if (use-region-p)
          (region-end)
        (progn
          (re-search-forward "\n[ \t]*\n" nil "move")
          (re-search-backward "\n[ \t]*\n" nil "move")
          (point)))
      (if current-prefix-arg t nil))))
  (let (($changedItems '())
        ($findReplaceMap
         (if @entity-to-char-p
             ;; this to prevent creating a replacement sequence out of blue
             [
              ["&amp;" "螽⛫1"] ["&lt;" "螽⛫2"] ["&gt;" "螽⛫3"]
              ["螽⛫1" "&"] ["螽⛫2" "<"] ["螽⛫3" ">"]
              ]
           [ ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ]
           )))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (let ( (case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while (search-forward (elt $x 0) nil t)
               (push (format "%s %s" (point) $x) $changedItems)
               (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")
               (overlay-put (make-overlay (- (point) (length (elt $x 1))) (point)) 'font-lock-face '(:foreground "red"))))
           $findReplaceMap))))))


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

(defun +insert-org-link-in-current-directory ()
  "Insert an Org link to a file in the current directory, the description is without directory and extension"
  (interactive)
  (let ((files (directory-files "." nil ".org" t)))
    (if files
        (let* ((selected-file (consult--read files
					     :prompt "Chose a file: "
					     ))
               (org-link (concat "[[file:" selected-file "][" (file-name-sans-extension (file-name-nondirectory selected-file)) "]]")))
          (insert org-link))
      (message "No files in the current directory."))))


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
(defcustom buku-db-file "~/.local/share/buku/bookmarks.db"
  "the buku bookmark database file")

(defun hsk/consult-buku--build-tag-string (raw)
  (let ((str (substring raw 0 (1- (length raw)))))
    (put-text-property 0 (length str) 'face '(:foreground red) str)
    str))

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
    (setq files (directory-files-recursively "~/Documents/castlemaybe/content"  "\\.\\(org\\|md\\)$"))
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
   (consult--read (find-sh-scripts-recursively renpy_games_location))))

(provide 'init-utils)
