(add-hook 'text-mode-hook 'abbrev-mode)
(setq save-abbrevs 'silently)

;; (advice-add
;;  'abbrev-insert
;;  :after
;;  (lambda (&rest args)
;;    (delete-char -1))
;;  '((name . "no-self-insert")))

(provide 'init-abbrev)
