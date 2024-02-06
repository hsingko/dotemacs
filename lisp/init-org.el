;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
;; cache easygpg key
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; log time after DONE, necessory ox-hugo
(setq org-log-done 'time)
;; org-download configuration
(setq org-startup-folded 'overview)


(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

;; ?? 
(setq org-M-RET-may-split-line nil)
(setq org-insert-heading-respect-content t)
;;

(use-package org-indent
  :after org
  :ensure nil
  :diminish)

(use-package org-journal
  :after org
  :commands (org-journal-new-entry)
  :config
  (setq org-journal-file-type 'yearly
	org-journal-file-format "%Y.org.age"
	org-journal-file-header "#+TITLE: Year %Y Journal"
	org-journal-date-prefix "* "
	org-journal-date-format "%a, %Y-%m-%d"
	org-journal-time-prefix "** "
	org-journal-encrypt-journal nil
	org-journal-dir (expand-file-name "journal" org-directory)
	))

(use-package org-download
  :commands (org-download-clipboard org-download-image)
  :init
  ;; (dir-locals-set-class-variables 'org-download-hugo-directory
  ;; 				  '((org-mode . ((org-download-image-dir . "./images")))))
  ;; (dir-locals-set-directory-class
  ;;  "~/Documents/Blog/content/" 'org-download-hugo-directory)
  :config
  (setq org-download-display-inline-images nil)
  (setq-default org-download-heading-lvl nil)
  ;; (setq-default org-download-image-dir (expand-file-name "images" org-directory))
  (setq-default org-download-image-dir "./images")
  (setq org-download-backend "wget")
  (setq org-download-abbreviate-filename-function (lambda (fn) fn)) ; use original filename
  (setq org-download-timestamp "%y%m%dT%H%M%S-")
  (defun dummy-org-download-annotate-function (link)
    "")
  (setq org-download-annotate-function
	#'dummy-org-download-annotate-function)
  )
(defun my/org-download-file-path ()
  (interactive)
  (org-download-image
   (concat "file://" (current-kill 0)))
  )

(setq org-agenda-files `(
			 ,(expand-file-name "gtd.org" org-directory)
			 ,(expand-file-name "capture.org" org-directory)
			 ))

(setq org-file-apps-gnu '((remote . emacs)
                          (system . "open %s")
                          ("ps.gz" . "gv %s")
                          ("eps.gz" . "gv %s")
                          ("dvi" . "xdvi %s")
                          ("fig" . "xfig %s")
			  ("webm" . "mpv %s")
                          (t . "open %s")
                          (".png" . "open %s")
			  ))



;; long line wrap
;; see: https://stackoverflow.com/questions/950340/how-do-you-activate-line-wrapping-in-emacs/950406#950406
;;; org-modern
(setq org-fontify-quote-and-verse-blocks t)


;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-startup-with-inline-images nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (python . t)
   (scheme . t)
   ))
(setq org-confirm-babel-evaluate nil)

;; hide drawers when cycle headline visibility
(add-hook 'org-cycle-hook #'org-cycle-hide-drawers)

(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))


(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
(setq org-file-apps '((directory . emacs)
                      (auto-mode . emacs)
                      ("\\.png\\'" . "xdg-open %s")
                      ("\\.jpg\\'" . "xdg-open %s")
                      ;; Add more image file extensions if needed
                      ))

(setq org-link-file-path-type 'absolute)

;; (setq org-preview-latex-image-directory (expand-file-name "images/" org-directory))

(setq org-log-into-drawer t) ;https://stackoverflow.com/questions/63797643/hide-org-todo-state-changes-in-drawer-or-properties


;; org-capture
(setq org-capture-templates nil)


(eval-after-load 'org
  '(progn
     (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)))


(setq org-hide-emphasis-markers t)

(eval-after-load 'org
  '(set-face-attribute 'org-quote nil :slant 'italic))

(provide 'init-org)
