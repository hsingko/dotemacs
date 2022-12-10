(use-package org-journal)

(setq org-journal-file-type 'yearly
      org-journal-file-format "%Y.org"
      org-journal-file-header "#+TITLE: Year %Y Journal"
      org-journal-date-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-time-prefix "** "
      org-journal-encrypt-journal t
      )

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(setq org-journal-dir "~/Documents/org/journal/")
