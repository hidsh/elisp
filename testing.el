;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; testing.el --- now testing
;;;
;;; now testing functions
;;;

;;
;; dired
;;
(defun my-dired-exit ()
  (interactive)
  (let ((buf (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (kill-buffer buf)))

(defun my-dired-do-trash ()
  )

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map (kbd "C") 'dired-do-chmod)  ; chmod
             (define-key dired-mode-map (kbd "m") 'dired-do-rename) ; mv
             (define-key dired-mode-map (kbd "M") nil)
             (define-key dired-mode-map (kbd "c") 'dired-do-copy)   ; cp
             (define-key dired-mode-map (kbd "q") 'my-dired-exit)
             (define-key dired-mode-map (kbd "a") 'dired-toggle-marks)))

;;
;; tabbar
;;
(require 'tabbar)
(tabbar-mode 1)
;; タブ上でマウスホイール操作無効
(tabbar-mwheel-mode -1)

;; グループ化しない
(setq tabbar-buffer-groups-function nil)

;; 左に表示されるボタンを無効化
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))


;; タブに表示させるバッファの設定
(defvar my-tabbar-displayed-buffers
  '("*scratch*")
  "*Regexps matches buffer names always included tabs.")

(defun my-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my-tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt my-tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))

(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

(global-set-key [(f12)]       'tabbar-forward-tab)
(global-set-key [(f11)]       'tabbar-backward-tab)

(defun my-tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (with-current-buffer buffer
        (kill-buffer)))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)))

(setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
(setq tabbar-select-tab-function 'my-tabbar-buffer-select-tab)

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format " [%s] " (tabbar-tab-tabset tab))
                  (format " %s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; 外観変更
(set-face-attribute 'tabbar-default nil
 :family "Lucida Grande" :height 130
 :background "#bebdbe")


(set-face-attribute 'tabbar-unselected nil
 :background "#bebdbe"
 :foreground "#3d3d3d"
 :box nil)

(set-face-attribute 'tabbar-selected nil
 :background "#22232a"
 :foreground "#e9e9e9"
 :box nil)

(setq tabbar-separator '(0.2)) ;; タブの長さ
(set-face-attribute 'tabbar-separator nil
 :background "#6c6c6c")

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
(setq sgml-electric-tag-pair-mode 1)

(add-hook 'html-mode-hook
          '(lambda ()
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

(defalias 'r 'quickrun)
;; (defadvice time-stamp (around time-stamp-around activate)
;;   (let (buffer-undo-list)
;;     ad-do-it))

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
(defun term ()
  (interactive)
  (let* ((shell-name "*shell*"))
    (when (get-buffer-window shell-name)
      (let* ((shell-buf (get-buffer "*shell*"))
             (shell-proc (get-buffer-process shell-buf)))
        (kill-process (get-buffer-process shell-buf))
      	(sit-for 0.5)
        (kill-buffer shell-buf)))
    (unless (one-window-p)
      (delete-other-windows))
    (split-window-below (- (window-height) 15))
    (select-window (next-window))
    (shell)
    (with-current-buffer shell-name
      (goto-char (point-max)))))

(defun term-kill ()
  (interactive)
  (let* ((shell-buf (get-buffer "*shell*"))
         (shell-proc (get-buffer-process shell-buf)))
    (kill-process shell-proc)
    (sit-for 0.5)
    (kill-buffer shell-buf)
    (unless (one-window-p)
      (delete-window (selected-window)))))
  
(add-hook 'shell-mode-hook
          '(lambda ()
             (define-key shell-mode-map "\C-xk" 'term-kill)))

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
  (while (string-match
          "[０-９Ａ-Ｚａ-ｚ]+"
          (buffer-substring start end))
    (save-excursion
      (japanese-hankaku-region――
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
(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

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
    (shell-command-to-string (format "ln -sf %s%s %s.latest" dir fn dir))
    (message "saved: %s%s" dir fn)))

(defun save-buffer-lisp-int ()
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (save-scratch-buffer)
    (call-interactively 'my-save-buffer)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map "\C-x\C-s" 'save-buffer-lisp-int)))


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


;;
;; powerline - https://github.com/milkypostman/powerline
;;
(require 'powerline)
(powerline-default)

;; active color
(set-face-background 'mode-line         "#FF0066")   ; pink
(set-face-foreground 'mode-line         "#FFFFDC")   ; near-white
(set-face-background 'powerline-active1 "#FF6699")   ; light-pink
(set-face-foreground 'powerline-active1 "#272821")   ; near-white
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
  (read-event)
  (transient-mark-mode 0))

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

(global-set-key (kbd "C-M-f") 'my-toggle-fullscreen)



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
 


;; for python


;;
;;
;;

(provide 'testing)
;;; testing.el ends here
