;;; chinese.el -*- lexical-binding: t; -*-
;;; chinese
(set-language-environment 'utf-8)

;; 行首输入 < 自动切换英文
(defun +rime-predicate-line-begin-with-left-arrow ()
  (and (<= (point) (save-excursion (back-to-indentation) (point)))
       (or
       (= #x3c rime--current-input-key)
       (= #x7c rime--current-input-key)
       (= #x28 rime--current-input-key)
        )
       ))

(defun +rime-predicate-meow-mode-p ()
  (and (fboundp 'meow-mode)
       (or (meow-normal-mode-p)
           (meow-motion-mode-p)
	   (meow-beacon-mode-p)
	   (meow-keypad-mode-p)
           )))

(defun +rime-predicates-basic ()
  "The basic necessary predicates combination."
  (or
   ;(rime-predicate-evil-mode-p)
      (+rime-predicate-meow-mode-p)
      (rime-predicate-ace-window-p)
      (rime-predicate-hydra-p)
      (+rime-predicate-button-at-point-p)
      ;; 行首切换 ascii-mode 在写中文文章的时候不好用，因为会遇到双引号开头段落的情况
      ;; (rime-predicate-punctuation-line-begin-p)
      (+rime-predicate-line-begin-with-left-arrow)
      ))

(defun +rime-predicate-button-at-point-p ()
  "Detect whether the point is a button.
  \"Button\" means that positon is not editable.
  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (button-at (point)))

(defun +rime-predicate-after-special-punctuation-p ()
  "If the cursor is after a string prefixed a special punctuation.
  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "[@:][a-zA-Z0-9-_~]*$" string))))

(defun +rime-predicate-beancount-p ()
  "Predicate input state in `beancount-mode'.
  Detect whether current buffer's `major-mode' is `beancount-mode',
  and the cursor is in the comments or strings.
  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (derived-mode-p 'beancount-mode)
       (not (or (nth 3 (syntax-ppss))
                (nth 4 (syntax-ppss))))))

(setq-default rime-disable-predicates
              '(+rime-predicates-basic
                 ;; rime-predicate-org-in-src-block-p
                 rime-predicate-org-latex-mode-p
                 rime-predicate-punctuation-after-space-cc-p
                 rime-predicate-punctuation-after-ascii-p))
(setq-default rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p
                 rime-predicate-space-after-cc-p
                 rime-predicate-after-ascii-char-p))

(setq rime-show-candidate 'posframe)
(setq rime-posframe-properties nil)
(setq rime-posframe-style 'horizontal)
(setq rime-show-preedit 'inline)
(use-package rime
  :custom
  (default-input-method "rime")
  ;; :hook
  ;; ((after-init kill-emacs) . (lambda ()
  ;;                              (when (fboundp 'rime-lib-sync-user-data)
  ;;                                (ignore-errors (rime-sync)))))

  )

(defun +rime--posframe-display-content-a (args)
  "给 `rime--posframe-display-content' 传入的字符串加一个全角空
          格，以解决 `posframe' 偶尔吃字的问题。"
  (cl-destructuring-bind (content) args
    (let ((newresult (if (string-blank-p content)
                         content
                       (concat content "　"))))
      (list newresult))))

(if (fboundp 'rime--posframe-display-content)
    (advice-add 'rime--posframe-display-content
                :filter-args
                #'+rime--posframe-display-content-a)
  (error "Function `rime--posframe-display-content' is not available."))

;; 中文断行问题
(setq word-wrap-by-category t)


(global-set-key "\M-m" #'rime-inline-ascii)
