;;; testing.el --- now testing
;;;
;;; now testing functions
;;;

;;
;; powerline - https://github.com/milkypostman/powerline
;;

(defun mm ()
  (interactive)
  (format "%S" mode-line-format))


(require 'powerline)
(powerline-default)

;; active
(set-face-background 'mode-line         "#FF0066")   ; pink
(set-face-foreground 'mode-line         "#FFFFDC")   ; near-white
(set-face-background 'powerline-active1 "#FF6699")   ; light-pink
(set-face-foreground 'powerline-active1 "#272821")   ; near-white
(set-face-background 'powerline-active2 "#CDC0B0")   ; sand
(set-face-foreground 'powerline-active2 "#272821")   ; near-black

;; inactive
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
  (if (buffer-modified-p) "mod " ""))

'(
;; モードラインに現在の関数名を表示
(which-function-mode 1)
(set-face-foreground 'which-func "Gray50")
(set-face-italic-p 'which-func t)


(defpowerline powerline-count-lines-and-chars
  (if (region-active-p)
      (format "(%3d:%4d)"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))

(defpowerline powerline-which-func
  (progn
    (which-function-mode 1)
    which-func-format))


(setq-default mode-line-format
'("%e"
 (:eval
  (let* ((active (powerline-selected-window-active))
         (mode-line (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (lhs (list
               (powerline-raw "%Z" nil 'l)
               ;; (powerline-buffer-size nil 'l)
               (powerline-buffer-id nil 'l)
               (powerline-raw " ")
               (powerline-arrow-right mode-line face1)
               (when (boundp 'erc-modified-channels-object)
                 (powerline-raw erc-modified-channels-object face1 'l))
               (powerline-major-mode face1 'l)
               (powerline-process face1)
               (powerline-minor-modes face1 'l)
               (powerline-raw " " face1)
               (powerline-arrow-right face1 face2)
               (powerline-view face2 'l)
               ))
         (rhs (list
               (powerline-raw global-mode-string face2 'r)
               ;; (powerline-which-func face2 'r)
               (powerline-vc face2)
               (powerline-raw " " face2)
               (powerline-arrow-left face2 face1)
               (powerline-raw " " face1)
               (powerline-narrow face1 'r)
               (powerline-count-lines-and-chars face1 'r)
               (powerline-raw "%4l" face1 'r)
               (powerline-raw ":" face1)
               (powerline-raw "%3c" face1 'r)
               (powerline-raw (format "%6d" (point)) face1 'r)
               (powerline-arrow-left face1 mode-line)
               (powerline-raw " ")
               (powerline-modified)
               (powerline-raw "%6p" nil 'r)
               ;; (powerline-hud face2 face1)
               (powerline-raw " ")
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

(global-set-key "\C-x\C-e" 'eval-region-or-last-sexp)
;(defalias 'ev 'eval-region-or-last-sexp)

;;
;;
;;
(defun my-mark-whole-buffer ()
  "Usefull mark-whole-buffer."
  (interactive)
  (push-mark (point-min))
  (goto-char (point-max))
  (message "Selected all"))

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
(global-set-key "\M-v" 'my-meta-v)



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
'(

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

)

;; for python


;;
;;
;;

(provide 'testing)
;;; testing.el ends here
