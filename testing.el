;;; testing.el --- now testing
;;;
;;; now testing functions
;;;

;;
;;
;;
(defun eval-region-or-last-sexp ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'eval-region
     'eval-last-sexp)))

(global-set-key "\C-x\C-e" 'eval-region-or-last-sexp)
(defalias 'ev 'eval-region-or-last-sexp)

;;
;;
;;
(defun my-mark-whole-buffer ()
  "Usefull mark-whole-buffer."
  (interactive)
  (push-mark (point-min))
  (goto-char (point-max))
  (message "Selected all"))

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
;;
;;
(defun my-keyboard-quit ()
  (interactive)
  (unless view-mode
    (view-mode-enter))
  (keyboard-quit))

(global-set-key "\C-g" 'my-keyboard-quit)

;;
;; count lines and chars on mode line
;;
(defun count-lines-and-chars ()
  (if (region-active-p)
      (format "[%3d:%4d]"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))

(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;;
;; full screen
;;
(defun my-toggle-fullscreen ()
  "Toggle fullscreen."
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
;; zoom font
;;
(global-set-key (kbd "C-M-=") 'text-scale-increase)
(global-set-key (kbd "C-M--") 'text-scale-decrease)
(global-set-key (kbd "C-M-0") '(lambda () (interactive) (text-scale-set 0)))


;;
;;
;;
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
