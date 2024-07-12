(use-package doct)

;; (setq org-capture-templates
;;       (doct '(("Parent" :keys "p"
;;                :file "~/example.org"
;;                :prepend t
;;                :template ("* %{todo-state} %^{Description}"
;;                           ":PROPERTIES:"
;;                           ":Created: %U"
;;                           ":END:"
;;                           "%?")
;;                :children (("First Child"  :keys "1"
;;                            :headline   "One"
;;                            :todo-state "TODO"
;;                            :hook (lambda () (message "\"First Child\" selected.")))
;;                           ("Second Child" :keys "2"
;;                            :headline   "Two"
;;                            :todo-state "NEXT")
;;                           ("Third Child"  :keys "3"
;;                            :headline   "Three"
;;                            :todo-state "MAYBE"))))))


(setq org-capture-templates
	     (doct `(("Todo" :keys "t"
		      :file ,(expand-file-name "inbox.org" org-directory)
		      :template ("* TODO %^{Description}"
				 ":PROPERTIES:"
				 ":Crated: %U"
				 ":END:"
				 "%?")))))


(provide 'init-org-capture)
;;; init-org-capture.el ends here
