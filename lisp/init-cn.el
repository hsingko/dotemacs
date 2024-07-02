;;; chinese
(set-language-environment 'utf-8)

;; 行首输入 < 自动切换英文
(defun +rime-predicate-line-begin-with-left-arrow ()
  (and (<= (point) (save-excursion (back-to-indentation) (point)))
       (or
	(= #x3c rime--current-input-key)
	(= #x7c rime--current-input-key)
	(= #x28 rime--current-input-key))))

(defun +rime-predicates-basic ()
  "The basic necessary predicates combination."
  (or
   ;; (+rime-predicate-meow-mode-p)
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
                rime-predicate-punctuation-after-ascii-p
		rime-predicate-after-ascii-char-p ;; 为了使用 abbrev 模式
		))

(if (featurep 'meow)
    (add-to-list 'rime-disable-predicates '+rime-predicate-meow-mode-p))


(setq-default rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p
                rime-predicate-space-after-cc-p))

(setq rime-show-candidate 'minibuffer)
(setq rime-posframe-properties nil)
(setq rime-posframe-style 'horizontal)
(setq rime-show-preedit t)
(use-package rime
  :custom
  ;;; 切换方案：https://emacs-china.org/t/emacs-rime/12048/54
  (default-input-method "rime")
  :config
  ;; (bind-key "C-`" 'rime-send-keybinding rime-mode-map)
  (add-hook 'kill-emacs-hook (lambda ()
			       (ignore-errors (rime-lib-finalize))))
  )

;; 中文断行问题
(setq word-wrap-by-category t)


;;; ace-pin 通过首字母在文字间定位
(use-package ace-pinyin
  :diminish
  :hook
  (org-mode . ace-pinyin-mode)
  (markdown-mode . ace-pinyin-mode))


;; 基于中文分词的跳跃功能
(use-package cns
  :load-path "git/cns"
  :config
  (setq cns-prog (expand-file-name "cns/cnws" user-emacs-directory))
  (setq cns-dict-directory (expand-file-name "cns/dict" user-emacs-directory))
  :hook
  (text-mode . cns-mode))

(use-package unicad)

(provide 'init-cn)

