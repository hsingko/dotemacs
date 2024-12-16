(use-package hydra)
(use-package org-fc
  :load-path "git/org-fc"
  :commands (org-fc-review org-fc-type-normal-init org-fc-type-cloze-init org-fc-review-all org-fc-review-buffer)
  :custom (org-fc-directories '("~/Documents/org"))
  )


(provide 'init-fc)
