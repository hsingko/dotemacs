(use-package ace-pinyin
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
	   (sentence (replace-regexp-in-string word (format "/%s/" word)
					       (replace-regexp-in-string
						"\n" "" raw))))
      (write-region
       (format "* %s\n + %s\n" word sentence)
       nil
       (expand-file-name "glossary.org" org-directory)
       'append))))

(setf (alist-get ?b avy-dispatch-alist) '+avy-capture-glossary)
