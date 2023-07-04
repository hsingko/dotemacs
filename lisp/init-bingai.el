(use-package websocket)
(use-package async-await)
(use-package aichat-bingai
  :load-path "git/emacs-aichat"
  :config
  (setq aichat-bingai-proxy "127.0.0.1:7890")
  (setq aichat-debug t)
  (setq aichat-bingai-cookies-file (expand-file-name "bingai-cookie.json" user-emacs-directory)))

(provide 'init-bingai)
