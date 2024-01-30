(use-package hydra)
(use-package org-fc
  :load-path "git/org-fc"
  :custom (org-fc-directories '("~/Documents/org"))
  :config
  (require 'org-fc-hydra))


(provide 'init-fc)
