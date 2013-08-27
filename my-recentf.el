;;; 
;;; my-recentf.el
;;;
;(require "")
(provide 'my-recentf)

;; (export '(my-recentf-mode-hook my-recentf-mode-map my-recentf-mode))
(defvar my-recentf-mode-hook nil)
(defvar my-recentf-buf-name "*my-recentf*")
(defvar my-recentf-directory-face nil)
(defvar my-recentf-window-conf nil)

;;
;; keymap
;;
(defvar my-recentf-mode-map nil)
(unless my-recentf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return]    'my-recentf-action-key-enter)
    (define-key map "\C-xk"     'my-recentf-kill-buffer)
    (define-key map "q"         'my-recentf-kill-buffer)
    (define-key map "\C-x\C-q"  'my-recentf-enter-edit-mode)
    (setq my-recentf-mode-map map)))

(defvar my-recentf-edit-map nil)
(unless my-recentf-edit-map
  (let ((map (copy-keymap text-mode-map)))
    (define-key map "\C-x\C-s"  'my-recentf-exit-edit-mode)
    (define-key map "\C-xk"     'my-recentf-discard-changes)
    (define-key map "\C-x\C-q"  'my-recentf-exit-edit-mode)
    (setq my-recentf-edit-map map)))

;;
;; util
;;
(defun my-recentf-enumulate-file-name ()
  (dolist (e recentf-list)
    (insert e "\n")))

(defun my-recentf-set-directory-face ()
  (when my-recentf-directory-face
    (goto-char (point-min))
    (while (re-search-forward "\\(.*/\\)$" nil t nil)
      (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                   'face my-recentf-directory-face))))

(defun my-recentf-get-current-line ()
  (save-excursion
    (let ((beg (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (buffer-substring beg end))))

(defun my-recentf-find-file ()
    (let ((fn (my-recentf-get-current-line)))
      (if (file-exists-p fn)
          (progn
            (my-recentf-update-list fn)
            (kill-buffer my-recentf-buf-name)
            (find-file fn))
          (message (concat "not found: " fn)))))

(defun my-recentf-update-list (e)
  (let ((l (reverse (set-exclusive-or recentf-list (list e) :test #'string=))))
    (setq recentf-list (push e l))))

(defun my-recentf-create-list ()
  (split-string (buffer-substring (point-min) (point-max)) "\n" nil " \t"))

(defun my-recentf-delete-empty-line ()
  (save-excursion 
    (goto-char (point-min))
    (perform-replace "^[ @\t]*\n" "" nil t))) ; ‹ó”’s‚ðíœ

(defun my-recentf-delete-not-exist ()
  (flet ((not-exist-p (x) (if (and (windows-local-path-p x) (not (file-exist-p x)))
                              t nil)))
    (let ((out (delete-if #'not-exist-p (my-recentf-create-list))))
      (delete-region (point-min) (point-max))
      (dolist (e out)
        (insert e "\n")))))

;; (defun my-recentf-upcase-drive-letter ()
;;   (let ((out   (mapcar #'upcase-drive-letter (my-recentf-create-list))))
;;     (delete-region (point-min) (point-max))
;;     (dolist (e out)
;;       (insert e "\n"))))

(defun my-recentf-subst-tilda (l)
  (let ((directory-abbrev-alist `((,(concat "\\`" (getenv "HOME")) . "~"))))
    (mapcar #'(lambda (x) (abbreviate-file-name x)) l)))

(defun my-recentf-cleanup-buffer ()
  (save-excursion
    (my-recentf-delete-empty-line)
    (my-recentf-delete-not-exist)
    ;; (my-recentf-upcase-drive-letter)
    ))

;;
;; command
;;
(defun my-recentf-action-key-enter ()
  (interactive)
  (if buffer-read-only
      (my-recentf-find-file)
    (newline)))

(defun my-recentf-save ()
  (interactive)
  (if (buffer-modified-p)
      (let ((ro buffer-read-only))
        (setq buffer-read-only nil)
        (my-recentf-cleanup-buffer)
        (setq recentf-list (my-recentf-subst-tilda (my-recentf-create-list)))
        (set-buffer-modified-p nil)
        (setq buffer-read-only ro)
        (message "saved."))
    (message "no need to save.")))
  

(defun my-recentf-kill-buffer ()
  (interactive)
  (kill-buffer my-recentf-buf-name)
  (set-window-configuration my-recentf-window-conf))

(defun my-recentf-discard-changes ()
  (interactive)
  (set-buffer-modified-p nil)           ; --> not modified
  (kill-buffer (current-buffer))
  (set-window-configuration my-recentf-window-conf))

(defun my-recentf-enter-edit-mode ()
  (interactive)
  (use-local-map my-recentf-edit-map)
  (setq buffer-read-only nil)
  (message "entered to edit mode"))

(defun my-recentf-exit-edit-mode ()
  (interactive)
  (my-recentf-save)
  (use-local-map my-recentf-mode-map)
  (setq buffer-read-only t)
  (message "exit from edit mode"))
  

(defun my-recentf ()
  (interactive)
  (when (get-buffer my-recentf-buf-name)
    (kill-buffer my-recentf-buf-name))
  (set-buffer (get-buffer-create my-recentf-buf-name))
  (my-recentf-enumulate-file-name)
  (my-recentf-set-directory-face)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (my-recentf-mode)
  (setq my-recentf-window-conf (current-window-configuration))
  (delete-other-windows)
  (switch-to-buffer my-recentf-buf-name))

(defun my-recentf-mode ()
  ;(kill-all-local-variables)
  (setq buffer-mode 'my-recentf-mode)
  (setq mode-name "my-recentf")
  (use-local-map my-recentf-mode-map)
  (setq need-not-save t)
  (setq auto-save nil)
  (setq kept-undo-information t)
  ;(make-local-variable 'highlight-keyword)
  ;(setq highlight-keyword nil)
  (run-hooks 'my-recentf-mode-hook))

(global-set-key "\M-r" 'my-recentf)

;;; my-recentf.el ends here
