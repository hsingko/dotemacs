(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(setq dired-deletion-confirmer #'y-or-n-p)

(defun +find-file-in-config()
  (interactive)
  (cd "~/.config/emacs/")
  (call-interactively #'find-file))

;; display start up performance
(defun +display-startup-time ()
  (message "emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'+display-startup-time)

(defun +delete-this-file()
  "kill this file and buffer with no confirm"
  (interactive)
  (delete-file (buffer-file-name))
  (kill-this-buffer))

(defun +rename-this-file()
  "TODO"
  (interactive)
  (rename-file (buffer-file-name)))
