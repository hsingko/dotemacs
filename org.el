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
(setq-default org-download-image-dir "./images")
(defun dummy-org-download-annotate-function (link)
  "")
(setq org-download-annotate-function
      #'dummy-org-download-annotate-function)
(defun my/org-download-file-path ()
  (interactive)
  (org-download-image
   (concat "file://" (current-kill 0)))
  )


(setq org-agenda-files '("~/Documents/org"))

(add-to-list
 'org-file-apps
     '(
     ("\\.pdf\\'" . system)
   ))


(org-link-set-parameters "zotero" :follow
                         (lambda (zpath)
                           (browse-url
                            ;; we get the "zotero:"-less url, so we put it back.
                            (format "zotero:%s" zpath))))


(use-package "org-modern")
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(org-indent-mode t)



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; turn off line display in org-mode markdown-mode text-mode


;;; org-modern
(global-org-modern-mode)
;; apperance
(setq org-modern-table nil)




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
