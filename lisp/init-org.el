;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
;; cache easygpg key
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; log time after DONE, necessory ox-hugo
(setq org-log-done 'time)
;; org-download configuration


(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0)
(setq org-M-RET-may-split-line nil)

(use-package org-indent
  :after org
  :ensure nil
  :diminish)

(use-package org-journal
  :commands (org-journal-new-entry)
  :config
  (setq org-journal-file-type 'yearly
      org-journal-file-format "%Y.org"
      org-journal-file-header "#+TITLE: Year %Y Journal"
      org-journal-date-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-time-prefix "** "
      ;; org-journal-encrypt-journal t
      org-journal-dir (expand-file-name "journal" org-directory)
      )
  )

(use-package org-download
  :commands (org-download-clipboard org-download-image)
  :config
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir (expand-file-name "images" org-directory))
  (setq org-download-backend "wget")
  (setq org-download-abbreviate-filename-function (lambda (fn) fn)) ; use original filename
  (setq org-download-timestamp "%Y%m%dT%H%M%S-")
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
			 ,(expand-file-name "read.org" org-directory)
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
(global-visual-line-mode)
;;; org-modern
(global-org-modern-mode)
;; apperance
(setq org-modern-table nil)
(setq org-modern-checkbox nil)
(setq org-fontify-quote-and-verse-blocks t)


;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(400))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t)
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (python . t)
   ;; (javascript . t)
   ))
(setq org-confirm-babel-evaluate nil)

; hide drawers when cycle headline visibility
(add-hook 'org-cycle-hook #'org-cycle-hide-drawers)


(setq org-capture-templates
      '((
	"g" "Glossary" entry (file "glossary.org")
	"* %^{word}\n%a\n%t\n + %^{sentence}")))

(add-to-list 'org-capture-templates
	     '("l" "Leetcode" entry (file+headline
				     "~/Documents/org/leetcode/index.org" "Daily")
	       "* TODO %^{NO}. %^{Name} \n#+include: %\\1.py src python\n%?"))

(add-to-list 'org-capture-templates
	     '("t" "todo" entry (file+headline
				 "~/Documents/org/todo.org" "Inbox")
	       "* TODO %?\n%u\n%f"))



(add-hook 'org-mode-hook (lambda ()
			   (toggle-input-method)
			   ))


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


(use-package ox-hugo
  :after ox
  :config
;;; this seems fix ox-hugo exports freeze problem
;;; see: https://github.com/kaushalmodi/ox-hugo/discussions/651
  (setq org-element-use-cache nil)  
  )

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(setq org-preview-latex-image-directory (expand-file-name "images/" org-directory))

(setq org-log-into-drawer t) ;https://stackoverflow.com/questions/63797643/hide-org-todo-state-changes-in-drawer-or-properties


(provide 'init-org)
