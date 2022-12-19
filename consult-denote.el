;;; consult-denotes.el --- Manage denotes with consult -*- lexical-binding: t -*-
(require 'consult)
(setq consult-denote-file-match ".+\\(org\\|md\\)$")
(setq consult-denote--all-sources nil)
(setq consult-denote--history nil)

(defface consult-notes-sep '((t (:inherit (consult-separator))))
  "Face for separator in `consult-notes'."
  :group 'faces)

(defun consult-denote--make-source (name char dir)
  "Return a notes source list suitable for `consult--multi'.
	NAME is the source name, CHAR is the narrowing character,
	and DIR is the directory to find notes."
  (let ((idir (propertize (file-name-as-directory dir) 'invisible t)))
    `(
      :name     ,(propertize name 'face 'consult-notes-sep)
      :narrow   ,char
      :category 'consult-denote
      ;; :face     consult-file
      ;; :annotate ,(apply-partially consult-notes-annotate-note-function name)
      :items    ,(lambda () (mapcar (lambda (f) (propertize f 'path (expand-file-name f dir)))
	        		    ;; filter files that glob *.*
	        		    (directory-files dir nil consult-denote-file-match)))
      :state    ,#'consult-denote--state
      ;; :action   ,(lambda (f) (find-file f) consult-notes-default-format)
      :new  ,(lambda (cand)
	       (consult-denote--new-note cand dir)) ;; maybe better use narrow key?
      )))


(defun consult-denote--file (cand)
  (format "%s" (get-text-property 0 'path cand)))

(defun consult-denote--state ()
  "File preview for denote files."
  (let ((open (consult--temporary-files))
	(state (consult--file-state)))
    (lambda (action cand)
      (unless cand
	(funcall open))
      (funcall state action (and cand
	                         (consult-denote--file cand))))))


(defun consult-denote ()
  (interactive)
  (consult-denote--make-all-source)
  (let ((selected (consult--multi consult-denote--all-sources
	                          :require-match
	                          (confirm-nonexistent-file-or-buffer)
	                          :prompt "Denotes: "
	                          :history 'consult-denote--history
				  )))
    ;; For non-matching candidates, fall back to buffer-file creation.
    (unless (plist-get (cdr selected) :match)
      ;; (funcall consult-notes-file-action (car selected))
      (message (car selected))
      )))

(defun consult-denote--make-all-source ()
  (add-to-list 'consult-denote--all-sources
	       (consult-denote--make-source
		"Inbox"
		?i
		denote-directory))
  (consult-denote--make-sub-directory))

(defun consult-denote--make-sub-directory()
  (dolist (subdir (denote-directory-subdirectories))
    (let* ((name (file-relative-name subdir denote-directory))
	   (narrow (aref name 0)))
      (add-to-list
       'consult-denote--all-sources
       (consult-denote--make-source
	name
	narrow
	subdir)))))

(defun consult-denote--new-note (cand directory)
  "Create new note with Denote with title CAND.
	Input \"foo\", then create \"id-foo\", file type is determined by
	`denote-file-type', choose manually when `denote-prompts' includes
	'file-type, or simply include the extension; \"foo.txt\", creates
	\"id-foo.txt\."
  (let* ((f (expand-file-name cand denote-directory))
	 (f-dir (file-name-directory f))
	 (f-name-base (file-name-base f))
	 (file-type (consult-denote--extension-file-type f))
	 keywords date template)
    (dolist (prompt denote-prompts)
      (pcase prompt
	('keywords (setq keywords (denote-keywords-prompt)))
	('file-type (setq file-type (denote-file-type-prompt)))
	('subdirectory (setq subdirectory (denote-subdirectory-prompt)))
	('date (setq date (denote-date-prompt)))
	('template (setq template (denote-template-prompt)))))
    (denote (string-trim f-name-base) keywords file-type directory date template)))


(defun consult-denote--extension-file-type (f)
  "Return denote file-type of F."
  (pcase (file-name-extension f)
    ("org" "org")
    ("md" "markdown-toml")
    ("txt" "text")))


;; see: https://github.com/mclear-tools/consult-notes/issues/16#issuecomment-1308852518
(vertico--define-sort (alpha-desc) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string> string>)
(vertico-multiform-mode)
(setq vertico-multiform-commands
      '((consult-denote (vertico-sort-function . vertico-sort-alpha-desc))))
