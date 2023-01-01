(use-package ace-pinyin
  :diminish
  :init (ace-pinyin-global-mode +1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(setq dired-deletion-confirmer #'y-or-n-p)

;; display start up performance
(defun +display-startup-time ()
  (message "emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'+display-startup-time)


(load "~/.config/emacs/file.el")

(defun cnfonts-insert-fontname ()
  "Select a valid font name, and insert at point."
  (interactive)
  (let ((all-fonts (font-family-list))
        fonts choose)
    (dolist (font all-fonts)
      (push (substring-no-properties
             (decode-coding-string font 'gbk))
            fonts)
      (push (substring-no-properties
             (decode-coding-string font 'utf-8))
            fonts))
    (setq fonts (delete-dups fonts))
    (setq choose (completing-read
                  "Which font name do you want to insert? "
                  (if (yes-or-no-p "Only show font names with Chinese? ")
                      (cl-remove-if
                       #'(lambda (x)
                           (not (string-match-p "\\cc" x)))
                       fonts)
                    fonts)))
    (when choose
      (insert (format "\"%s\"" choose)))))


(defun +avy-capture-glossary (pt)
  (save-excursion
    (goto-char pt)
    (let* ((word (word-at-point t))
	   (raw (sentence-at-point t))
	   (sentence (replace-regexp-in-string word (format " /%s/ " word)
					       (replace-regexp-in-string
						"\n" "" raw))))
      (write-region
       (format "* %s\n + %s\n" word sentence)
       nil
       (expand-file-name "glossary.org" org-directory)
       'append))))

(setf (alist-get ?b avy-dispatch-alist) '+avy-capture-glossary)

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-al --group-directories-first")
  (define-key dired-mode-map (kbd "C-c C-w") #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))
  )

(defun +read-file-lines (fp)
  (with-temp-buffer
    (insert-file-contents fp)
    (split-string (buffer-string) "\n" t)))

(random t)
(setq random-word-file "~/.config/emacs/random-words.txt")
(defun +random-words-1 ()
  (interactive)
  (let* ((pool (+read-file-lines random-word-file))
	 (limit (length pool))
	 (n (random (+ 1 limit)))
	 (word (nth n pool)))
    (message word)))

(defun +random-words-2 ()
  (interactive)
  (let* ((pool (+read-file-lines random-word-file))
	 (limit (length pool))
	 (n1 (random (+ 1 limit)))
	 (n2 (random (+ 1 limit)))
	 (word1 (nth n1 pool))
	 (word2 (nth n2 pool)))
    (message (format "%s, %s" word1 word2))))
(defun +random-words-3 ()
  (interactive)
  (let* ((pool (+read-file-lines random-word-file))
	 (limit (length pool))
	 (n1 (random (+ 1 limit)))
	 (n2 (random (+ 1 limit)))
	 (n3 (random (+ 1 limit)))
	 (word1 (nth n1 pool))
	 (word2 (nth n2 pool))
	 (word3 (nth n3 pool)))
    (message (format "%s, %s, %s" word1 word2 word3))))
  

(setq eww-retrieve-command '("readable"))

(use-package fish-mode)

(electric-pair-mode 1)


(setq delete-by-moving-to-trash t)
