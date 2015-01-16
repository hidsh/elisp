;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; testing.el --- now testing
;;;

;;
;;
;;
(global-set-key "\C-x\C-d" 'dired)


;;
;; alias
;;
(defalias 'revert 'revert-buffer)
(defalias 'view 'view-mode)


;;
;; evil
;;
(require 'evil)
(evil-mode 1)
(defalias 'evil 'evil-mode)
(setq evil-move-cursor-back nil)

(define-key evil-motion-state-map (kbd "SPC") #'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "<S-SPC>") #'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map "\C-o" nil)
(define-key evil-motion-state-map "q" nil)
(define-key evil-motion-state-map (kbd "TAB") nil)

(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "q" nil)
(define-key evil-insert-state-map "\C-y" 'yank)

(define-key evil-insert-state-map "\C-r" 'search-backward)
;; (define-key evil-insert-state-map "\M-j" #'evil-force-normal-state) ; ESC 
(define-key evil-insert-state-map "j" #'evil-maybe-exit) ; jj --> ESC 
(evil-define-command evil-maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p))
        (entry-key ?j)
        (exit-key ?j))
    (insert entry-key)
    (let ((evt (read-event (format "Insert %c to exit insert state" exit-key) nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(evil-ex-define-cmd "wq" 'my-save-kill-current-butffer)
(defun my-save-kill-current-butffer ()
  :repeat nil
  (interactive)
  (save-buffer)
  (kill-buffer (current-buffer)))



(require 'evil-numbers)
(define-key evil-normal-state-map "=" #'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" #'evil-numbers/dec-at-pt)

;; cursor color
(setq evil-default-cursor 'hollow
      evil-normal-state-cursor '("white")
      evil-insert-state-cursor '("#FF0066" (bar . 3)))


;;
;; for tabbar
;;
;; taken from http://www.emacswiki.org/emacs/TabBarMode#toc12
;; (when (require 'tabbar+ nil t)
;;   ;; Enable tabbars globally:
;;   (tabbar-mode 1)
;;   ;; I use this minor-mode mainly as a global mode (see below):
;;   (define-minor-mode tabbar-on-custom-mode
;;     "Display tabbar on selected modes only."
;;     :init-value t
;;     :lighter nil
;;     :keymap nil
;;     (if tabbar-on-custom-mode
;;         ;; filter is enabled
;;         (if (memq major-mode '(term-mode 
;;                                eshell-mode 
;;                                dired-mode 
;;                                help-mode 
;;                                apropos-mode 
;;                                Info-mode 
;;                                Man-mode))
;;             (tabbar-local-mode -1)
;;           (tabbar-local-mode 1))
;;       ;; always activate tabbar locally when we disable the minor mode:
;;       (tabbar-local-mode -1)))

;;   (defun tabbar-on-custom-mode-on ()
;;     "Turn on tabbar if current buffer is a terminal."
;;     (unless (minibufferp) (tabbar-on-custom-mode 1)))
;;   ;; Define a global switch for the mode. Note that this is not set for buffers
;;   ;; in fundamental mode.
;;   ;;
;;   ;; I use it 'cause some major modes do not run the
;;   ;; `after-change-major-mode-hook'...
;;   (define-globalized-minor-mode global-tabbar-on-custom-mode
;;     tabbar-on-custom-mode tabbar-on-custom-mode-on)
;;   ;; Eventually, switch on this global filter for tabbars:
;;   (global-tabbar-on-custom-mode 1))


;;
;; mod (orig:discrete.el)
;; add: open-finder
(defun my-find-file (filename &optional wildcards)
  (interactive (my-find-file-interactive-arg "Find file: "))
  (let ((curr (buffer-file-name)))
    (if (string= filename curr)
        (open-finder-1 (file-name-directory curr) (file-name-nondirectory curr))
      (find-file filename wildcards))))


;;
;; set read only after saving
;;
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (unless buffer-read-only
;;               (toggle-read-only))))

;;
;; rinari for ruby on rails
;;
;; Interactively Do Things (highly recommended, but not strictly required)
;(require 'ido)
;(ido-mode t)
;; Rinari
;(add-to-list 'load-path "~/src/rinari")
;(add-to-list 'load-path "~/git-clone/rinari")
;(require 'rinari)

;;; rhtml-mode
(add-to-list 'load-path "~/src/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
    (lambda () (rinari-launch)))

;;
;; font size (SHIFT +/-)
;;
(global-set-key "\M-+" 'text-scale-increase)
(global-set-key "\M-_" 'text-scale-decrease)


;;
;; toggle-read-only
;;
;; (add-hook 'find-file-hook
;;      '(lambda ()
;;         (toggle-read-only)))

;; (defun my-bs ()
;;   (interactive)
;;   (if buffer-file-name
;;     (toggle-read-only)
;;     (call-interactively 'backward-delete-char-untabify))
;;   (message ""))

;; (global-set-key (kbd "M-DEL") 'my-bs) ; del key
;; (global-set-key [backspace] 'my-bs) ; del key


;;
;; disable menu key
;;
(global-unset-key "\M-`")               ; menu keys

;;
;; jedi for python
;;
;; (add-to-list load-path "~/elisp/elpa/epc-20130803.2228")
(require 'epc)
(require 'auto-complete-config)
(require 'python)

;;;;; PYTHONPATH上のソースコードがauto-completeの補完対象になる ;;;;;
;; (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages")
(setenv "PYTHONPATH" "/opt/local/lib/python2.7/site-packages")
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;;
;; expand-region
;;
;; (add-to-list 'load-path "~/elisp/expand-region/")
;; (require 'expand-region)
;; (global-set-key "\M-=" 'er/expand-region)  ; win + =


;;
;; c-mode compile
;;  [F7]でコンパイル(バッファを全部保存して，make all します)
;;  [F4]でコンパイルエラー行へジャンプ
;;  [F1]でカーソル下の単語について マニュアルを開く <-- disabled
;;  SHIFT+[F7] でリビルド(make clean all)
;;  SHIFT+[F4] で一つ前のエラー行へジャンプ
;;
(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
                ("\\.cc$" . c++-mode)
                ("\\.cpp$". c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.c$"  . c-mode)
                ("\\.h$"  . c++-mode))
              auto-mode-alist))

(add-hook 'c-mode-common-hook
     '(lambda ()
        (require 'vc-hooks)
        (setq completion-mode t)
        ;; make のオプションは聞いてこない
        (setq compilation-read-command nil)
        ;; make するとき 全バッファを自動的にsaveする
        (setq compilation-ask-about-save nil)
        ;; (define-key c-mode-base-map [f1] 'manual-entry)
        (define-key c-mode-base-map [f4] 'next-error)
        (define-key c-mode-base-map [(shift f4)] 'previous-error)
         (define-key c-mode-base-map [f7] 'compile)
        (define-key c-mode-base-map [(shift f7)] 
          '(lambda () 
             (interactive)
             ;;(require 'compile)
             ;;(save-some-buffers (not compilation-ask-about-save) nil)
             (compile-internal "make clean all" "No more errors")))
      ))

;; 
;; auto highlihgt symbol
;;
(global-unset-key [M-right])
(global-unset-key [M-left])

(global-set-key [M-up] 'ahs-backward)
(global-set-key [M-down] 'ahs-forward)


;;
;; show ascii table
;;
(defun ascii ()
  (interactive)
  (list-charset-chars 'ascii))

;;
;; color directory string in completion buffer
;;
(defadvice completion-list-mode-finish (after filename-completeion-adv activate)
  (when minibuffer-completing-file-name
    (save-excursion
      (while (re-search-forward "\\([^\n\t]+/\\)" nil t nil)
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face dired-directory-face)))))

;;
;; diff-buffers
;;
(defalias 'diff-buffers 'ediff-buffers)

;;
;; fixed comment-indent @ newcomment.el.gz
;;

(defun comment-indent (&optional continue)
  "Indent this line's comment to `comment-column', or insert an empty comment.
If CONTINUE is non-nil, use the `comment-continue' markers if any."
  (interactive "*")
  (comment-normalize-vars)
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or (and continue comment-continue)
		      (and empty block-comment-start) comment-start))
	 (ender (or (and continue comment-continue "")
		    (and empty block-comment-end) comment-end)))
    (unless starter (error "No comment syntax defined"))
    (beginning-of-line)
    (let* ((eolpos (line-end-position))
	   (begpos (comment-search-forward eolpos t))
	   cpos indent)
      (if (and comment-insert-comment-function (not begpos))
	  ;; If no comment and c-i-c-f is set, let it do everything.
	  (funcall comment-insert-comment-function)
	;; An existing comment?
	(if begpos
	    (progn
	      (if (and (not (looking-at "[\t\n ]"))
		       (looking-at comment-end-skip))
		  ;; The comment is empty and we have skipped all its space
		  ;; and landed right before the comment-ender:
		  ;; Go back to the middle of the space.
		  (forward-char (/ (skip-chars-backward " \t") -2)))
	      (setq cpos (point-marker)))
	  ;; If none, insert one.
	  (save-excursion
	    ;; Some `comment-indent-function's insist on not moving
	    ;; comments that are in column 0, so we first go to the
	    ;; likely target column.
	    (indent-to comment-column)
	    ;; Ensure there's a space before the comment for things
	    ;; like sh where it matters (as well as being neater).
	    (unless (memq (char-before) '(nil ?\n ?\t ?\s))
	      (insert ?\s))
	    (setq begpos (point))
	    (insert starter)
	    (setq cpos (point-marker))
	    (insert ender)))
	(goto-char begpos)
	;; Compute desired indent.
	(setq indent (save-excursion (funcall comment-indent-function)))
	;; If `indent' is nil and there's code before the comment, we can't
	;; use `indent-according-to-mode', so we default to comment-column.
	(unless (or indent (save-excursion (skip-chars-backward " \t") (bolp)))
	  (setq indent comment-column))
	(if (not indent)
	    ;; comment-indent-function refuses: delegate to line-indent.
	    (indent-according-to-mode)
	  ;; If the comment is at the right of code, adjust the indentation.
	  (unless (save-excursion (skip-chars-backward " \t") (bolp))
	    (setq indent (comment-choose-indent indent)))
	  ;; Update INDENT to leave at least one space
	  ;; after other nonwhite text on the line.
	  (save-excursion
	    (skip-chars-backward " \t")
	    (unless (bolp)
	      ;; (setq indent (max indent (1+ (current-column))))))
	      (setq indent (max indent comment-column))))                           ; <<---- !!!
	  ;; If that's different from comment's current position, change it.
	  (unless (= (current-column) indent)
	    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
	    (indent-to indent)))
	(goto-char cpos)
	(set-marker cpos nil)))))

;;
;; popwin
;;
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("*Apropos*") popwin:special-display-config)                       ;; Apropos

;;
;; slime
;;
(when nil
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")
  (setq slime-lisp-implementations
        `((sbcl ("/opt/local/bin/sbcl"))
          (abcl ("/opt/local/bin/abcl"))
          (clisp ("/opt/local/bin/clisp"))))

  (setq slime-default-lisp 'clisp)
  (require 'slime)
  ;; (slime-setup  '(slime-repl slime-asdf slime-fancy slime-banner))
  (slime-setup  '(slime-repl slime-fancy slime-banner))

  (push '("*slime-apropos*") popwin:special-display-config)                 ;; Apropos
  (push '("*slime-macroexpansion*") popwin:special-display-config)          ;; Macroexpand
  (push '("*slime-description*") popwin:special-display-config)             ;; Help
  (push '("*slime-compilation*" :noselect t) popwin:special-display-config) ;; Compilation
  (push '("*slime-xref*") popwin:special-display-config)                    ;; Cross-reference
  (push '(sldb-mode :stick t) popwin:special-display-config)                ;; Debugger
  (push '(slime-repl-mode) popwin:special-display-config)                   ;; REPL
  (push '(slime-connection-list-mode) popwin:special-display-config)        ;; Connections



  (let ((hooks '(help-mode-hook apropos-mode-hook)))
    (dolist (h hooks)
      (add-hook h '(lambda () (view-mode 1)))))
)
)

;;
;; my-kill-buffer
;;
(defun my-kill-buffer (arg)
  (interactive "P")
  (if arg
      (dolist (b (buffer-list))
        (kill-buffer b))
    (kill-buffer (buffer-name))))

(global-set-key "\C-xk" 'my-kill-buffer)


;;
;; my-customized-goto-line
;;
(defun my-goto-line ()
  (interactive)
  (call-interactively 'goto-line)
  (recenter))

(global-set-key "\M-g" 'my-goto-line)

;;
;; my-open-at-point
;;
(defun my-open-at-point ()
  (interactive)
  (let* ((url (bounds-of-thing-at-point 'url))
         (file (bounds-of-thing-at-point 'filename))
         (path (expand-file-name (if file (buffer-substring-no-properties (car file) (cdr file)) "~"))))
    (cond (url (browse-url  (buffer-substring-no-properties (car url) (cdr url))))
          ((and file (file-directory-p path)) (call-interactively 'my-open-at-point-dir))
          (file (if (file-exists-p path)
                    (call-interactively 'my-open-at-point-file)
                  (message "not found:%s" path)))
          ((string= (buffer-name) "*scratch*") (let ((path "~/scratch/"))
                                                 (call-interactively 'my-open-at-point-dir)))
          ((not (buffer-file-name)) (message "This buffer has no file"))
          ((eq major-mode 'html-mode) (call-interactively 'my-open-at-point-html))
          (t (my-open-at-point-finder (buffer-file-name))))))

(defun my-open-at-point-dir (key)
  (interactive "copen by (d)ired or (f)inder?")
  (cond ((eq key ?d) (dired path))
        ((eq key ?f) (my-open-at-point-finder path))
        (t (message "canceled"))))

(defun my-open-at-point-file (key)
  (interactive "copen by (e)macs or (f)inder?")
  (cond ((eq key ?e) (find-file path))
        ((eq key ?f) (my-open-at-point-finder path))
        (t (message "canceled"))))

(defun my-open-at-point-html (key)
  (interactive "copen THIS FILE by (b)rowser or (f)inder?")
  (cond ((eq key ?b) (browse-url (buffer-file-name)))
        ((eq key ?f) (my-open-at-point-finder (buffer-file-name)))
        (t (message "canceled"))))

(defun my-open-at-point-finder (path)
  (let* ((dir (file-name-directory path))
         (file (file-name-nondirectory path))
         (script (concat
                  "tell application \"Finder\"\n"
                  "    make new Finder window to (POSIX file \"" dir "\")\n" 
                  "    set frontmost to true\n"
                  "    select POSIX file \"" path "\"\n"
                  "end tell")))
    (shell-command (concat "osascript -e '" script "'")))
  (message ""))

(global-set-key "\M-j" 'my-open-at-point) ; overwrite browse-url-of-find-file @discrete.el

;;
;; minibuffer history
;;
(define-key minibuffer-local-map "\C-n" 'next-history-element)
(define-key minibuffer-local-map "\C-p" 'previous-history-element)

;;
;; white-space
;;
(global-whitespace-mode 1)

;; スペースの定義は全角スペースとする。
(setq whitespace-space-regexp "\x3000+")

;; 改行の色を変更
(set-face-foreground 'whitespace-newline "gray40")

;; 半角スペースと改行を除外
(dolist (d '((space-mark ?\ ) (newline-mark ?\n)))
  (setq whitespace-display-mappings
        (delete-if
         '(lambda (e) (and (eq (car d) (car e))
                           (eq (cadr d) (cadr e))))
         whitespace-display-mappings)))

;; 全角スペースと改行を追加
(dolist (e '((space-mark   ?\x3000 [?\□])
             ;; (newline-mark ?\n     [?\u21B5 ?\n] [?$ ?\n])
             ))
  (add-to-list 'whitespace-display-mappings e))

;; 強調したくない要素を削除
(dolist (d '(face lines space-before-tab
                  indentation empty space-after-tab tab-mark))
  (setq whitespace-style (delq d whitespace-style)))

;;
;; ace-jump-mode
;;
(require 'ace-jump-mode)

(defun add-keys-to-ace-jump-mode (prefix c &optional mode)
  (define-key global-map
    (read-kbd-macro (concat prefix (string c)))
    `(lambda ()
       (interactive)
       (funcall (if (eq ',mode 'word)
                    #'ace-jump-word-mode
                  #'ace-jump-char-mode) ,c))))

; ESC/Win/App key
(loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-" c 'word))
(loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-" c 'word))
;; (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-M-" c))
;; (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-M-" c))
;; (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-M-" c))
;; (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-M-" c))
;; (loop for c from ?! to ?~ do (add-keys-to-ace-jump-mode "H-" c))

(setq mac-option-modifier 'hyper)

;;
;; スクロール
;;
(defun my-scroll-down ()
  (interactive)
  (if (> (window-start) (point-min))
      (scroll-down-command)
    (goto-char (point-min))))

(defun my-scroll-up ()
  (interactive)
  (if (< (window-end) (point-max))
      (scroll-up-command)
    (goto-char (point-max))))

(global-set-key "\M-p" 'my-scroll-down)
(global-set-key "\M-n" 'my-scroll-up)

(substitute-key-definition 'scroll-up-command   'my-scroll-up   (current-global-map))
(substitute-key-definition 'scroll-down-command 'my-scroll-down (current-global-map))

(substitute-key-definition 'View-scroll-page-forward  'my-scroll-up   view-mode-map)
(substitute-key-definition 'View-scroll-page-backward 'my-scroll-down view-mode-map)


;;
;; find-file-find
;;
(require 'find-file-find)

;;
;; nxml-mode の変なキーバインドをはずす
;;
(require 'nxml-mode)
(define-key nxml-mode-map "\M-h" nil)

;;
;; my-just-one-space
;;
(defun delete-spaces-after-point ()
  (while (= 0 (syntax-class (syntax-after (point))))
    (delete-char 1)))

(defun count-white-spaces-at-point ()
  (let ((nl 0)
        (nr 0))
    (save-excursion
      (while (and (< (point) (line-end-position)) (= 0 (syntax-class (syntax-after (point)))))
        (incf nr)
        (forward-char 1))
      (while (and (= 0 (syntax-class (syntax-after (1- (point))))) (< (line-beginning-position) (point)))
        (incf nl)
        (backward-char 1)))
    (+ nl nr)))

(defun my-just-one-space ()
  (interactive)
  (cond ((and (bolp) (eolp)) (just-one-space))
        ((and (eolp) (= (line-beginning-position) (1- (point)))) (backward-delete-char 1))
        ((and (bolp) (= (line-end-position) (1+ (point)))) (delete-char 1))
        ((and (not (eq last-command 'my-just-one-space)) (and (char-after) (= (char-after) ?\ )))
         (delete-spaces-after-point))
        ((= (count-white-spaces-at-point) 1)
         (backward-delete-char 1))
        (t
         (just-one-space))))

(global-set-key "\M- " 'my-just-one-space)

;;
;; my-end-of-line
;;
(require 'newcomment)                   ; for comment-search-forward
(defun skip-backward-comment-and-space ()
  (let* ((bol (line-beginning-position))
         (b 0))
    (end-of-line)
    (while (and (<= bol (point))
                (< (point-min) bol)
                (or (member (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))
                    (= 0 (syntax-class (syntax-after (point))))))    ; white space
      (backward-char 1)
      (setq b 1))         
    (forward-char b)))

(defun my-end-of-line-1 ()
  "本文末尾 -> end of line　の順に移動"
  (let* ((curr (point))
    tail com)
  (save-excursion
    (setq tail (progn (skip-backward-comment-and-space) (point))))
    ;; (setq com  (progn (beginning-of-line) (comment-search-forward eol t) (point)))) ; コメントの先頭位置 *
  (cond
   ((= tail (line-beginning-position)) (end-of-line))
   ((< curr tail) (goto-char tail))
   ;; ((< curr com) (goto-char com))   ; コメントの先頭に移動 *
   ((= curr (line-end-position)) (goto-char tail))
   (t (end-of-line)))))

(defun my-end-of-line ()
  (interactive)
  (cond ((minibuffer-p) (end-of-line))
        ((= (line-end-position) (point-max)) (end-of-line))
        (t (my-end-of-line-1))))

(global-set-key "\C-e" 'my-end-of-line)

;; 変数をハイライト
;; (emacs 24.3.50 では標準に組み込まれる予定)
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(setq ahs-idle-interval 0.5)
(custom-set-variables '(ahs-default-range (quote ahs-range-whole-buffer))) ; C-x C-a: replace all

(defun toggle-auto-highlight-symbol ()
  (interactive)
  (if auto-highlight-symbol-mode
      (setq auto-highlight-symbol-mode nil)
    (setq auto-highlight-symbol-mode t)))

(defalias 'v 'toggle-auto-highlight-symbol)

(set-face-background 'ahs-plugin-whole-buffer-face "steel blue")
(set-face-foreground 'ahs-plugin-whole-buffer-face "white")


;;
;; bs-show w/ arg
;;
(defvar my-bs-show-window-conf nil)
(defun my-bs-show ()
  (interactive)
  (bs-show 1))

(global-set-key "\C-x\C-b" 'my-bs-show)

;;
;; カーソル行をハイライト
;;
(defalias 'h 'hl-line-mode)
(global-hl-line-mode 0)
(set-face-background 'hl-line "black")

;; global-hl-line-mode するとスクロールがガクガクするので、バッファ開くときに個別に hl-line-mode を add-hook する
(dolist (hook '(my-recentf-mode-hook find-file-hook dired-mode-hook))
  (add-hook hook '(lambda () (hl-line-mode 1))))


;;
;; oblosete な関数を無効化する
;;
(defmacro make-local-hook (vars))


;;
;; my-recentf
;;
(require 'my-recentf)
(recentf-mode 1)                        ; for recentf-list

(add-hook 'my-recentf-mode-hook
          '(lambda ()
     (define-key my-recentf-mode-map "h" 'backward-char)
     (define-key my-recentf-mode-map "j" 'next-line)
     (define-key my-recentf-mode-map "k" 'previous-line)
     (define-key my-recentf-mode-map "l" 'forward-char)
     (define-key my-recentf-mode-map "g" 'beginning-of-buffer)
     (define-key my-recentf-mode-map "G" 'end-of-buffer)
     (define-key my-recentf-mode-map "0" 'my-beginning-of-line)
     (define-key my-recentf-mode-map "4" 'end-of-line)
     ;; (define-key my-recentf-mode-map "w" 'my-recentf-enter-edit-mode)  ; why doesnot work??
     (define-key my-recentf-mode-map [?\ ]    'my-scroll-up)
     (define-key my-recentf-mode-map [?\S-\ ] 'my-scroll-down)
     (define-key my-recentf-mode-map "n" 'my-scroll-down)
     (define-key my-recentf-mode-map "/" 'my-recentf-isearch-forward)
     (define-key my-recentf-mode-map "s" 'my-recentf-isearch-forward)
     ;; (define-key my-recentf-mode-map "\M-e" 'recentf-edit-list)
     ;; (define-key my-recentf-mode-map "\M-\C-m" 'recentf-open-dired)
     (define-key my-recentf-mode-map [delete] 'my-recentf-enter-edit-mode)
     (define-key my-recentf-edit-map [delete] 'my-recentf-exit-edit-mode)))

(setq my-recentf-directory-face `((:foreground ,"#F1266F")))

(setq recentf-max-saved-items 8000)     ;TODO
(setq recentf-exclude '("\\.emacs-places$"))
(require 'recentf-ext)


;;
;; ql
;;
(defun ql ()
  (interactive)
  (let ((fn (buffer-file-name)))
    (when fn
      (start-process "ql-file" nil "qlmanage" "-p" fn))))

;;
;; keyboad macro
;;
(defalias 'macro-record-start 'start-kbd-macro)
(defalias 'macro-record-end   'end-kbd-macro)
(defalias 'macro-call         'call-last-kbd-macro) ; e.g. C-u 4 M-x macro-call

;;
;; web-mode
;;
(require 'web-mode)

;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           '(lambda ()
;;              (define-key web-mode-map "\M-]" 'web-mode-tag-match)
;;              (define-key web-mode-map "\M-\C-]" 'my-paren)
;;              (define-key web-mode-map "\M-d") 'web-mode-element-delete))

(defun my-paren-auto ()
  (interactive)
  (let ((pr '(?\( ?\[ ?\{))
        (pl '(?\) ?\] ?\})))
    (if (or (memq (char-after) pr) (memq (char-before) pl))
        (call-interactively 'my-paren)
      (call-interactively 'web-mode-tag-match))))

(add-hook 'html-mode-hook
          '(lambda ()
             (define-key html-mode-map "\M-]" 'my-paren-auto)))

;;
;; JS2
;;
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;
;; sgml-close-tag
;;
(defadvice sgml-close-tag (around back-adv activate)
  (let ((pt (point)))
    ad-do-it
    (goto-char pt)))



;;
;; dired
;;
(setq delete-by-moving-to-trash t)

(defun my-dired-exit ()
  (interactive)
  (let ((buf (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (kill-buffer buf)))

(defun my-beginning-of-line-dired ()
 (interactive)
 (let ((curr (current-column))
       (ind  47))
   (cond
    ((= curr 0)   (move-to-column ind))
    ((< curr ind) (beginning-of-line))
    ((> curr ind) (move-to-column ind))
    (t (beginning-of-line)))))

(defun open-mac (path)
  (start-process "dired-open-mac" nil "open" path))

(defun quicklook-file (path)
  (interactive)
  (defvar cur nil)
  (defvar old nil)
  (setq old cur)
  (setq cur (start-process "ql-file" nil "qlmanage" "-p" path))
  (when old (delete-process old)))

(defun my-dired-open ()
  (interactive)
  (let ((exts-ql   '("jpeg" "jpg" "png" "gif"))
        (exts-open '("avi" "mkv" "mp4" "pdf"))
        (ext (file-name-extension (dired-get-file-for-visit))))
     (cond ((file-accessible-directory-p (dired-get-file-for-visit))
            (call-interactively 'dired-find-alternate-file))
           ((and ext (member (downcase ext) exts-ql))
            (funcall 'quicklook-file (dired-get-file-for-visit)))
           ((and ext (member (downcase ext) exts-open))
            (funcall 'open-mac (dired-get-file-for-visit)))
           (t
            (call-interactively 'dired-find-file-other-window)))))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "\C-m"  'my-dired-open)
             (define-key dired-mode-map (kbd "C") 'dired-do-chmod)  ; chmod
             (define-key dired-mode-map (kbd "m") 'dired-do-rename) ; mv
             (define-key dired-mode-map (kbd "M") nil)
             (define-key dired-mode-map (kbd "c") 'dired-do-copy)   ; cp
             (define-key dired-mode-map (kbd "q") 'my-dired-exit)
             (define-key dired-mode-map (kbd "a") 'dired-toggle-marks)
             (define-key dired-mode-map "\C-a" 'my-beginning-of-line-dired)))


;;
;; 最後のマークに移動
;;
(defun move-to-mark ()
      (interactive)
      (let ((pos (point)))
        (goto-char (mark))
        (push-mark pos)))

(global-set-key "\C-t" 'move-to-mark)

;;
;; html-mode
;;

(add-hook 'html-mode-hook
          '(lambda ()
             (setq sgml-electric-tag-pair-mode 1)
             (define-key html-mode-map "\M-t" 'sgml-close-tag)
             (define-key html-mode-map "\M-," 'insert-paren-gtlt)
             (define-key html-mode-map [M-left]  'sgml-skip-tag-backward)
             (define-key html-mode-map [M-right] 'sgml-skip-tag-forward)))


;;
;; バックアップの保存先を指定
;;
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/bak"))
        backup-directory-alist))

(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/bak/") t)))

(setq auto-save-list-file-prefix nil)
      
;;
;; quickrun
;;
(require 'quickrun)

(defalias 'run 'quickrun)
(defalias 'r 'quickrun)
;; (defadvice quickrun/make-sentinel (around back-window-adv activate)
;;   (ignore-errors
;;     (unwind-protect ad-do-it
;;       (set-window-start (selected-window) 1)
;;       (other-window -1)
;;       (quickrun/remove-temp-files))))


(add-hook 'quickrun-after-run-hook
          '(lambda ()
             (set-window-start (selected-window) 1)
             (other-window -1)
             (quickrun/remove-temp-files)))

;;
;; css-mode
;;
(setq cssm-indent-function 'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;;
;; JSON
;;
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(require 'flymake-json)
(add-hook 'json-mode-hook 'flymake-json-load)

;; (custom-set-faces
;;  '(flymake-errline ((((class color)) (:background "red"))))
;;  '(flymake-warnline ((((class color)) (:background "gold")))))


;;
;; my terminal (shell)
;;
;; (defun term ()
;;   (interactive)
;;   (let* ((shell-name "*shell*"))
;;     (when (get-buffer-window shell-name)
;;       (let* ((shell-buf (get-buffer "*shell*"))
;;              (shell-proc (get-buffer-process shell-buf)))
;;         (kill-process (get-buffer-process shell-buf))
;;       	(sit-for 0.5)
;;         (kill-buffer shell-buf)))
;;     (unless (one-window-p)
;;       (delete-other-windows))
;;     (split-window-below (- (window-height) 15))
;;     (select-window (next-window))
;;     (shell)
;;     (with-current-buffer shell-name
;;       (goto-char (point-max)))))
;;
;; (defun term-kill ()
;;   (interactive)
;;   (let* ((shell-buf (get-buffer "*shell*"))
;;          (shell-proc (get-buffer-process shell-buf)))
;;     (kill-process shell-proc)
;;     (sit-for 0.5)
;;     (kill-buffer shell-buf)
;;     (unless (one-window-p)
;;       (delete-window (selected-window)))))
;;  
;; (add-hook 'shell-mode-hook
;;           '(lambda ()
;;              (define-key shell-mode-map "\C-xk" 'term-kill)))

;;
;; replace to discrete.el
;;
(defun my-beginning-of-line ()
 (interactive)
 (let ((curr (current-column))
       (ind  (current-indentation)))
   (cond
    ((= curr 0)   (move-to-column ind))
    ((< curr ind) (beginning-of-line))
    ((> curr ind) (move-to-column ind))
    (t (beginning-of-line)))))

(global-set-key "\C-a" 'my-beginning-of-line)


;;
;; disable vc
;;
(setq vc-handled-backends '())


;;
;; query-replace in dired
;;
(defun my-dired-goto-top ()
  (goto-char (point-min))
  (when (re-search-forward "[0-9] \\(\\.\\.\\)$" nil t)
    (goto-char (match-beginning 1))))

(defun my-query-replace-dired ()
  (interactive)
  (my-dired-goto-top)
  (call-interactively 'my-query-replace)
  (my-dired-goto-top))

(define-key wdired-mode-map "\M-%" 'my-query-replace-dired)


;; ;;
;; ;; zencoding
;; ;;
;; (require 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode)
;; ;; (define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-line)
;; (define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-yas)
;; (defalias 'zen 'zencoding-mode)         ; ON/OFF

;; (defun my-zencoding-expand ()
;;   (interactive)
;;   (let ((exclude '(nil 10 32)))
;;     (if (memq (char-before) exclude)
;;         (indent-for-tab-command)
;;       (zencoding-expand-yas))))



;; ;;
;; ;; auto-save-buffers
;; ;;
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 0.5 t 'auto-save-buffers)   ; every 0.5sec idle && buffer-modified-p

;; ; 標準の自動バックアップを無効化しておく
;; (setq make-backup-files nil) ; e.g. hoge.txt~
;; (setq auto-save-default nil) ; e.g. #hoge.txt#

;; ; for undo
;; (add-hook 'write-file-hooks 'time-stamp)
;; (defadvice time-stamp (around time-stamp-around activate)
;;   (let (buffer-undo-list)
;;     ad-do-it))

;; (global-unset-key "\C-x\C-s")           ; save-buffer


;;
;; 全角英数 -> 半角 変換
;;
(defun hankaku-eisuu-region (start end)
 "選択範囲内の全角英数字を半角英数字に変換"
  (interactive "r")
  (require 'japan-util)
  (while (string-match
          "[０-９Ａ-Ｚａ-ｚ]+"
          (buffer-substring start end))
    (save-excursion
      (japanese-hankaku-region
       (+ start (match-beginning 0))
       (+ start (match-end 0))
       ))))

;;
(defun hankaku-eisuu-buffer ()
  "バッファ全体の全角英数字を半角英数字に変換"
  (interactive)
  (hankaku-eisuu-region (point-min) (point-max)))

(defalias 'zen-han-eisu 'hankaku-eisuu-buffer)

;;
;; my-query-replace
;;
(defun my-query-replace (arg)
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if arg 'query-replace-regexp 'query-replace))))

(global-set-key "\M-%" 'my-query-replace)

;;
;; リージョンをMarkdown向けのコードに整形
;;
(defun markdown-code-region (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (untabify beg end)
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward-regexp "\\(^.*\\)" nil t)
        (replace-match (concat "    " (match-string 0)))))))

;;
;; Ruby でも scratch-buffer で対話する
;;
(defun eval-region-ruby (beg end)
  (insert "\n###\n")
  (insert (with-output-to-string (shell-command-on-region beg end "ruby" standard-output))
          "\n"))

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (define-key "\C-x\C-e" 'eval-region-ruby)))

;;
;; Python でも scratch-buffer で対話する
;;
(defun eval-region-python (beg end)
  (insert "\n###\n")
  (let ((result (with-output-to-string (shell-command-on-region beg end "python" standard-output))))
    (insert result "\n")
    (when (string-match "SyntaxError: Non-ASCII character" result)
      (insert "You may need: # -*- coding: utf-8 -*-\n"))))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (define-key "\C-x\C-e" 'eval-region-python)))

; 結果をスクラッチバッファに出さずに、slime っぽく別のウィンドウに表示するのは下記のように。
; (defun eval-region-python (beg end)
;   (interactive "r")
;   (let ((buf "*Py Output*")
;         (curwin (selected-window)))
;     (shell-command-on-region beg end "python" buf)
;     (pop-to-buffer buf)
;     (select-window curwin)))


;;
;; メジャーモードによって eval-region を変更
;;
(setf eval-region-elisp (symbol-function 'eval-region))

(defun eval-region (beg end)
  (interactive "r")
  (funcall (cond ((eq major-mode 'lisp-interaction-mode)
                  eval-region-elisp)
                 ((eq major-mode 'emacs-lisp-mode)
                  eval-region-elisp)
                 ((eq major-mode 'python-mode)
                  'eval-region-python)
                 ((eq major-mode 'ruby-mode)
                  'eval-region-ruby))
           beg end))
;;
;; python (pre-installed python.el)
;;

;; ; paren completion
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (define-key python-mode-map "\"" 'electric-pair)
;;             (define-key python-mode-map "\'" 'electric-pair)
;;             (define-key python-mode-map "(" 'electric-pair)
;;             (define-key python-mode-map "[" 'electric-pair)
;;             (define-key python-mode-map "{" 'electric-pair)))
;; (defun electric-pair ()
;;   "Insert character pair without sournding spaces"
;;   (interactive)
;;   (let (parens-require-spaces)
;;     (insert-pair)))

; auto indent
;; (add-hook 'python-mode-hook '(lambda ()
;;                                (setq python-indent 4)
;;                                (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; ; flymake
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pychecker"  (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (load-library "flymake-cursor")

;; (global-set-key [f10] 'flymake-goto-prev-error)
;; (global-set-key [f11] 'flymake-goto-next-error)

;; ; pdb
;; (setq gud-key-prefix "\C-x\C-a")

;;
;; show full path on modeline
;;
(defvar mode-line-buffer-fullpath
              (list 'buffer-file-name
                    (propertized-buffer-identification "%12f")
                    (propertized-buffer-identification "%12b")))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; TODO: handle (DIRECTORY FILE ...) list value for dired-directory
            (setq mode-line-buffer-identification
                  ;; emulate "%17b" (see dired-mode):
                  '(:eval
                    (propertized-buffer-identification
                     (if (< (length default-directory) 17)
                         (concat default-directory
                                 (make-string (- 17 (length default-directory))
                                              ?\s))
                       default-directory))))))

(setq  mode-line-buffer-default mode-line-buffer-identification)

; not use
(defun toggle-mode-line-fullpath ()
  (interactive)
  (setq mode-line-buffer-identification
                (if (string= (format "%s" mode-line-buffer-identification)
                             (format "%s" mode-line-buffer-fullpath))
                    mode-line-buffer-default
                  mode-line-buffer-fullpath))
  ;; (force-mode-line-update)
  )

; currently using
(defun show-mode-line-fullpath (event)
  (interactive "e")
  (when (buffer-file-name)
    (select-window (posn-window (event-start event))) ; activate window
    (let ((wait-sec 5))
      (setq mode-line-buffer-identification mode-line-buffer-fullpath)
      (force-mode-line-update)
      (my-copy-buffer-file-name)                      ; copy path string to killring
      (sit-for wait-sec)
      (setq mode-line-buffer-identification mode-line-buffer-default)
      (force-mode-line-update)
      (message ""))))
  
    

(define-key mode-line-buffer-identification-keymap [mode-line mouse-1] 'show-mode-line-fullpath) ; left click

(set-face-attribute 'mode-line-highlight nil :box nil) ; remove box when hover mouse


;;
;; nop
;;
(defun nop ()
  (interactive))


;;
;; save scratch buffer
;;

;; (defun load-scratch-buffer ()
;;   (let ((dir "~/scratch/")
;;         (fn ".latest")
  

(defun save-scratch-buffer ()
  (let ((dir "~/scratch/")
        (fn (format-time-string "%04Y%02m%02d-%02H%02M%02S.el")))
    (save-excursion
      (widen)
      (write-region (point-min) (point-max) (concat dir fn)))
    (shell-command-to-string (format "ln -sf %s%s %slatest.el" dir fn dir))
    (message "saved: %s%s" dir fn)))

(defun save-buffer-lisp-int ()
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (save-scratch-buffer)
    (call-interactively 'my-save-buffer)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map "\C-x\C-s" 'save-buffer-lisp-int)))

(defun scratch-load-latest ()
  (interactive)
  (when (string= (buffer-name) "*scratch*")
    (erase-buffer)
    (let* ((path "~/scratch/latest.el")
           (truename (file-chase-links path)))
      (if (and (file-exists-p path)
               (file-exists-p truename))
          (progn
            (insert-file path)
            (message "loaded latest:%s" truename))
        (message "not exists:%s" path)))))

;;
;; open url string near the point using default www browser
;;
(defun browse-url-at-point ()
  (interactive)
  (let ((url-region (bounds-of-thing-at-point 'url)))
    (when url-region
      (browse-url (buffer-substring-no-properties (car url-region)
						  (cdr url-region))))))

(global-set-key "\M-\C-m" 'browse-url-at-point)      ; M-RET

;; modeline (mode name)
(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " ")
(setcar (cdr (assq 'global-whitespace-mode minor-mode-alist)) " ")
(setcar (cdr (assq 'auto-highlight-symbol-mode minor-mode-alist)) " HS")
(setq undo-tree-mode-lighter "")


;;
;; powerline - https://github.com/milkypostman/powerline
;;
(when (eq system-type 'darwin)
  (require 'powerline)
  (powerline-default)

  ;; active color
  (set-face-background 'mode-line         "#FF0066")   ; pink
  (set-face-foreground 'mode-line         "#FFFFDC")   ; near-white
  (set-face-background 'powerline-active1 "#FF6699")   ; light-pink
  (set-face-foreground 'powerline-active1 "#272821")   ; near-black
  (set-face-background 'powerline-active2 "#CDC0B0")   ; sand
  (set-face-foreground 'powerline-active2 "#272821")   ; near-black

  ;; inactive color
  (set-face-background 'mode-line-inactive  "#CCCC99") ; sand
  (set-face-foreground 'mode-line-inactive  "#272821") ; near-black
  (set-face-background 'powerline-inactive1 "#383838") ; near black
  (set-face-foreground 'powerline-inactive1 "#CCCCCC") ; light-gray
  (set-face-background 'powerline-inactive2 "#626262") ; dark-gray
  (set-face-foreground 'powerline-inactive2 "#CCCCCC") ; light-gray

  ;; View mode
  (defpowerline powerline-view
    (when view-mode "View"))

  (add-hook 'view-mode-hook
            '(lambda ()
               (setcar (cdr (assq 'view-mode minor-mode-alist)) "")))

  ;; modified-p
  (defpowerline powerline-modified
    (if (buffer-modified-p) "mod" ""))

  '( 
    ;; モードラインに現在の関数名を表示
    (which-function-mode 1)
    (set-face-foreground 'which-func "Gray50")
    (set-face-italic-p 'which-func t)

    (defpowerline powerline-which-func
      (progn
        (which-function-mode 1)
        which-func-format))
    )

  (defpowerline powerline-count-lines-and-chars
    (if (region-active-p)
        (format "(%3d:%4d)"
                (count-lines (region-beginning) (region-end))
                (- (region-end) (region-beginning)))
      ""))

  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (height 20)
                          (lhs (list
                                (powerline-raw "%Z" nil 'l)
                                ;; (powerline-buffer-size nil 'l)
                                (powerline-buffer-id nil 'l)
                                (powerline-raw " ")
                                (powerline-arrow-right mode-line face1 height)
                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object face1 'l))
                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-minor-modes face1 'l)
                                (powerline-raw " " face1)
                                (powerline-arrow-right face1 face2 height)
                                (powerline-view face2 'l)
                                ))
                          (rhs (list
                                (powerline-raw global-mode-string face2 'r)
                                ;; (powerline-which-func face2 'r)
                                (powerline-vc face2 'r)
                                (powerline-arrow-left face2 face1 height)
                                (powerline-raw " " face1)
                                (powerline-narrow face1)
                                (powerline-count-lines-and-chars face1)
                                (powerline-raw "%4l" face1 'r)
                                (powerline-raw ":" face1)
                                (powerline-raw "%3c" face1 'r)
                                (powerline-raw (format "%6d" (point)) face1 'r)
                                (powerline-arrow-left face1 mode-line height)
                                (powerline-raw " ")
                                (powerline-modified)
                                (powerline-raw " ")
                                (powerline-raw "%6p%8 ")
                                ;; (powerline-hud face2 face1)
                                ;; (powerline-raw "    " nil 'r)
                                )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs))))))

)
;;
;; タイトルバーにバッファ名かファイル名を表示
;;
(setq frame-title-format
      (if (buffer-file-name)
          (format "%%f - Emacs")
        (format "%%b - Emacs")))

;;
;; org-mode and remember
;;
(setq remember-data-file "~/memo/remember")    

(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory "~/memo/")
(setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-capture-templates
      '(("t" "Task" entry (file+headline nil "Inbox") "** TODO %?\n   %i\n   %a\n   %t\n   \n")
        ("b" "Bug"  entry (file+headline nil "Inbox") "** TODO %?   :bug:\n   %i\n   %a\n   %t\n   \n")
        ("i" "Idea" entry (file+headline nil "Idea")  "** %?\n   %i\n   %a\n   %t\n   \n")))

(add-hook 'org-capture-mode-hook
          '(lambda ()
             (view-mode-exit)
             (widen)
             (define-key org-capture-mode-map (kbd "q") 'org-capture-finalize)
             (define-key org-capture-mode-map (kbd "C-x C-k") 'org-capture-kill)))


(defalias 'm 'org-capture)


;;
;; my-delete-other-windows --> near the "switching window" at dot.emacs-gnu
;;
(defun my-delete-other-windows ()
  (interactive)
  (while (not (one-window-p))
    (let ((buf-name (buffer-name (window-buffer (next-window)))))
      (when (and (not (string-match "^\*scratch\*$" buf-name))
                 (string-match "\\(^ .+\\|^\*.\*$\\)" buf-name))
        (with-selected-window (selected-window)
          (select-window (next-window))
          (kill-buffer (window-buffer))))
      (delete-window (next-window)))))

(global-set-key (kbd "C-1") 'my-delete-other-windows)


;;
;; my-backup-diff
;;
(defun my-backup-choose-latest (l)
  (flet ((get-suffix-string (x)
            (let* ((ss (substring x (string-match "[0-9]*$" x)))
                   (s  (if (string< "" ss) (string-to-int ss) -1)))
              (cons s x))))
    (let ((lmax '(-2 . nil)))
      (dolist (e (mapcar 'get-suffix-string l))
        (when (< (car lmax) (car e))
          (setq lmax e)))
      (cdr lmax))))

(defun my-backup-diff-1 (fn)
  (let* ((fn (buffer-file-name))
         (l (directory-files (file-name-directory fn) t (format "%s\.%s[0-9]*" (file-name-nondirectory fn) my-backup-default-ext)))
         (old (my-backup-choose-latest l))
         (buf-name " diff")
         (frame-conf (current-frame-configuration))
         (ret 0))
    (with-output-to-temp-buffer buf-name
      (setq ret (call-process "diff" nil buf-name nil "-U 0" old fn)))
    (if (= ret 0)
        (progn
          (kill-buffer buf-name)
          (set-frame-configuration frame-conf)
          (message "vs %s: no differences" (file-name-nondirectory old)))
      (message "vs %s" (file-name-nondirectory old)))))

(defun my-backup-diff ()
  "カレントバッファと一つ前のバックアップファイルのdiffをとる"
  (interactive)
  (let ((fn (buffer-file-name)))
    (if fn
        (my-backup-diff-1 fn)
      (message "no file"))))

(defalias 'bak-diff 'my-backup-diff)

;;
;;
;;
(defun eval-region-or-last-sexp ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'eval-region
     'eval-last-sexp)))

;; (global-set-key "\C-x\C-e" 'eval-region-or-last-sexp)
;(defalias 'ev 'eval-region-or-last-sexp)

;;
;;
;;
(defun my-mark-whole-buffer ()
  "Usefull mark-whole-buffer."
  (interactive)
  (transient-mark-mode 1)
  (push-mark (point-min))
  (goto-char (point-max))
  (message "Selected all")
  (let ((ch (read-event)))
    (transient-mark-mode 0)
    ;; (cond ((= ch ?c) (message "!!!A"))
          ;; ((= ch ?b) (message "!!!B"))
          ;; (t (message "!!!X"))))    
))
(global-set-key "\C-x\C-a" 'my-mark-whole-buffer)
(global-set-key "\M-a"     'my-mark-whole-buffer)

(defun my-meta-substring ()
  (let ((s (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties (mark) (point)))))
    (substring s 0 (min (length s) 20))))

(defun my-meta-c ()
  (interactive)
  (cond ((eq last-command 'my-mark-whole-buffer)
         (call-interactively 'kill-ring-save)
         (message "Copied all"))
        ((region-active-p)
         (call-interactively 'kill-ring-save)
         (message "Copied '%s..." (my-meta-substring)))
        (t (call-interactively 'capitalize-word))))

(defun my-meta-x ()
  (interactive)
  (cond (buffer-read-only
         (call-interactively 'execute-extended-command))
        ((eq last-command 'my-mark-whole-buffer)
         (call-interactively 'kill-region)
         (message "Cut all"))
        ((and (region-active-p) (< (- (point) (mark)) 0))
         (message "invalid region."))
        ((and (region-active-p) (> (- (point) (mark)) 500))
         (when (y-or-n-p "large region. cut sure? ")
           (let ((s (my-meta-substring)))
             (call-interactively 'kill-region)
             (message "Cut '%s..." s))))
        ((region-active-p)
         (let ((s (my-meta-substring)))
           (call-interactively 'kill-region)
           (message "Cut '%s..." s)))
        (t
         (call-interactively 'execute-extended-command))))

(defun my-meta-v ()
  (interactive)
  (if buffer-read-only
      (message "buffer is read only.")
    (call-interactively 'yank)))

(global-set-key "\M-a" 'my-mark-whole-buffer)
(global-set-key "\M-c" 'my-meta-c)
(global-set-key "\M-x" 'my-meta-x)
;; (global-set-key "\M-v" 'my-meta-v)


;;
;; full screen
;;
(defun my-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
      (progn
        (set-frame-parameter nil 'fullscreen nil)
        (display-time-mode 0))
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (display-time-mode 1)))

(global-set-key (kbd "M-F") 'my-toggle-fullscreen)
;; (global-set-key (kbd "C-M-f") 'my-toggle-fullscreen)



;;
;; dired preview
;;
'(
(defvar dired-view-file-other-window-viewing-p nil)

(add-hook 'dired-mode-hook
          '(lambda ()
             (setq dired-view-file-other-window-viewing-p nil)))

(defun dired-view-file-other-window-1 ()
)
  


(defun dired-view-file-other-window ()
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (dired file))
      (save-selected-window
        (view-file-other-window file))
      )))

(defun dired-view-file-next (&optional reverse)
  (interactive)
  (let ((viewing-p dired-view-file-other-window-viewing-p))
    (when viewing-p
      (other-window 1)
      (kill-buffer (current-buffer))
      (delete-window))
    (if reverse (dired-previous-line 1)
      (dired-next-line 1))
    (when viewing-p (dired-view-file-other-window))))

(defun dired-view-file-previous ()
  (interactive)
  (dired-view-file-next 1))

(defun dired-view-file-scroll-page-forward (&optional reverse)
  (interactive)
  (save-selected-window
    (other-window 1)
    (if reverse (View-scroll-page-backward)
      (View-scroll-page-forward))))

(defun dired-view-file-scroll-page-backward ()
  (interactive)
  (dired-view-file-scroll-page-forward 1))


(defun dired-view-file-other-window-toggle ()
  (interactive)
  (setq dired-view-file-other-window-viewing-p (not dired-view-file-other-window-viewing-p))
  (if dired-view-file-other-window-viewing-p
      (progn
        (dired-view-file-other-window)
        ;; change key bindings
        (define-key dired-mode-map [?\ ]    'dired-view-file-scroll-page-forward)
        (define-key dired-mode-map [?\S-\ ] 'dired-view-file-scroll-page-backward)
        (define-key dired-mode-map "n"      'dired-view-file-scroll-page-backward))
    (delete-other-windows)
    ;; restore key bindings
    (define-key dired-mode-map [?\ ]    'dired-toggle-mark)
    (define-key dired-mode-map [?\S-\ ] 'dired-toggle-mark)
    (define-key dired-mode-map "n"      'dired-next-line)))


(define-key dired-mode-map (kbd "v") 'dired-view-file-other-window-toggle)
(define-key dired-mode-map (kbd "j") 'dired-view-file-next)
(define-key dired-mode-map (kbd "k") 'dired-view-file-previous)


)


;;
;; flymake
;;
(require 'flymake)

;; for javascript
(setq flymake-js-detect-trailing-comma t)

(defconst flymake-allowed-js-file-name-masks '(("\\.json$" flymake-js-init)
                                               ("\\.js$" flymake-js-init)))
(defcustom flymake-js-detect-trailing-comma t nil :type 'boolean)
(defvar flymake-js-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
(when flymake-js-detect-trailing-comma
  (setq flymake-js-err-line-patterns (append flymake-js-err-line-patterns
                                             '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)))))

(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "js" (list "-s" local-file))))
(defun flymake-js-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))
  (setq flymake-err-line-patterns flymake-js-err-line-patterns)
  (flymake-mode t))

(add-hook 'javascript-mode-hook '(lambda () (flymake-js-load)))

(global-set-key "\M-e" '(lambda ()
                           (interactive)
                           (let (line (line-number-at-pos))
                             (flymake-goto-next-error)
                             (when (equal line (line-number-at-pos))
                               (next-error)))))
(global-set-key "\M-E" '(lambda ()
                           (interactive)
                           (let (line (line-number-at-pos))
                             (flymake-goto-prev-error)
                             (when (equal line (line-number-at-pos))
                               (previous-error)))))
(global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)

;;
;; 田キーをhyperに
;;
;;(setq mac-option-modifier 'hyper)

;;
;; ESCキーをhyperに
;;
(define-key local-function-key-map [escape] 'event-apply-hyper-modifier)

(global-set-key (kbd "H-p") 'beginning-of-buffer)
(global-set-key (kbd "H-n") 'end-of-buffer)

;;
;; cursor-color
;;
(custom-set-faces '(cursor ((t (:background "#F92672")))))



(provide 'testing)
;;; testing.el ends here
