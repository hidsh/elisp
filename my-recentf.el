;;; 
;;; my-recentf.el
;;;
(require "util")
(provide "recentf")

(export '(*recentf-mode-hook* *recentf-mode-map* *recentf-mode*))
(defvar *recentf-mode-hook* nil)
(defvar *recentf-buf-name* "*recentf*")

;;
;; keymap
;;
(defvar *recentf-mode-map* nil)
(unless *recentf-mode-map*
  (setq *recentf-mode-map* (make-sparse-keymap))
  (define-key *recentf-mode-map* #\RET          'recentf-action-key-enter)
  (define-key *recentf-mode-map* '(#\C-x #\C-s) 'recentf-save)
  (define-key *recentf-mode-map* '(#\C-x #\k)   'recentf-kill-buffer)
  (define-key *recentf-mode-map* #\q            'recentf-kill-buffer))

;;
;; util
;;
(defun recentf-enumulate-file-nume ()
  (dolist (e *minibuffer-file-name-history*)
    (insert e "\n")))

(defun recentf-get-current-line ()
  (save-excursion
    (let ((beg (progn (goto-bol) (point)))
          (end (progn (goto-eol) (point))))
      (buffer-substring beg end))))

(defun recentf-find-file ()
    (let ((fn (recentf-get-current-line)))
      (if (file-exist-p fn)
          (progn
            (recentf-update-list fn)
            (delete-buffer *recentf-buf-name*)
            (find-file fn))
          (message (concat "not found: " fn)))))

(defun recentf-update-list (e)
  (let ((l (reverse (set-exclusive-or *minibuffer-file-name-history* (list e) :test #'string=))))
    (setq *minibuffer-file-name-history* (push e l))))

(defun recentf-create-list ()
  (split-string (buffer-substring (point-min) (point-max)) "\n" nil " \t"))

(defun recentf-delete-empty-line ()
  (save-excursion 
    (goto-char (point-min))
    (replace-buffer "^[ \t]*\n" "" :regexp t))) ; ãÛîíçsÇçÌèú

(defun recentf-delete-not-exist ()
  (flet ((not-exist-p (x) (if (and (windows-local-path-p x) (not (file-exist-p x)))
                              t nil)))
    (let ((out (delete-if #'not-exist-p (recentf-create-list))))
      (delete-region (point-min) (point-max))
      (dolist (e out)
        (insert e "\n")))))

(defun recentf-upcase-drive-letter ()
  (let ((out   (mapcar #'upcase-drive-letter (recentf-create-list))))
    (delete-region (point-min) (point-max))
    (dolist (e out)
      (insert e "\n"))))

(defun recentf-cleanup-buffer ()
  (save-excursion
    (recentf-delete-empty-line)
    (recentf-delete-not-exist)
    (recentf-upcase-drive-letter)))

;;
;; command
;;
(defun recentf-action-key-enter ()
  (interactive)
  (if buffer-read-only
      (recentf-find-file)
    (newline)))

(defun recentf-save ()
  (interactive)
  (if (buffer-modified-p)
      (let ((ro buffer-read-only))
        (setq buffer-read-only nil)
        (recentf-cleanup-buffer)
        (setq *minibuffer-file-name-history* (recentf-create-list))
        (set-buffer-modified-p nil)
        (setq buffer-read-only ro)
        (message "saved."))
    (message "no need to save.")))
  

(defun recentf-kill-buffer ()
  (interactive)
  (delete-buffer *recentf-buf-name*))

(defun recentf ()
  (interactive)
  (when (find-buffer *recentf-buf-name*)
    (delete-buffer *recentf-buf-name*))
  (set-buffer (get-buffer-create *recentf-buf-name*))
  (recentf-enumulate-file-nume)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (recentf-mode))

(defun recentf-mode ()
  ;(kill-all-local-variables)
  (setq buffer-mode 'recentf-mode)
  (setq mode-name "recentf")
  (use-keymap *recentf-mode-map*)
  (setq need-not-save t)
  (setq auto-save nil)
  (setq kept-undo-information t)
  ;(make-local-variable 'highlight-keyword)
  ;(setq highlight-keyword nil)
  (run-hooks '*recentf-mode-hook*))

(global-set-key '(#\C-x #\r #\r) 'recentf)

;;; recentf.l ends here
