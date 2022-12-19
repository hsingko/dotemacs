;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
;; cache easygpg key
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; log time after DONE, necessory ox-hugo
(setq org-log-done 'time)
;; org-download configuration
(use-package "org-download")
(require 'org-download)
(setq-default org-download-heading-lvl nil)
(setq-default org-download-image-dir "~/Documents/org/images")
(setq org-download-backend "wget")
(defun dummy-org-download-annotate-function (link)
  "")
(setq org-download-annotate-function
      #'dummy-org-download-annotate-function)
(defun my/org-download-file-path ()
  (interactive)
  (org-download-image
   (concat "file://" (current-kill 0)))
  )


(setq org-agenda-files `(
			 ,(expand-file-name "gtd.org" org-directory)
			 ,(expand-file-name "readlog.org" org-directory)
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


(org-link-set-parameters "zotero" :follow
                         (lambda (zpath)
                           (browse-url
                            ;; we get the "zotero:"-less url, so we put it back.
                            (format "zotero:%s" zpath))))


(use-package "org-modern")
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
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
      org-image-actual-width '(300))

(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

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


(add-hook 'org-mode-hook (lambda ()
			   (toggle-input-method)
			   ))

(use-package org-pomodoro)
