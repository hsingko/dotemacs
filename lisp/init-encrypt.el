(use-package age
  :demand t
  :custom
  (age-program "age")
  (age-default-identity "~/.ssh/id_rsa")
  (age-default-recipient "~/.ssh/id_rsa.pub")
  :config
  (age-file-enable))

(provide 'init-encrypt)
;;; init-age.el ends here
