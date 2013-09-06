;;; 
;;; my-recentf.el
;;;
(require 'cl)
(require 'view)
(provide 'my-recentf)

;; customize
(defvar my-recentf-ignore-list
  '("^/ssh:"))

(defvar my-recentf-exclude-list
  '("\.pdf$" "\.pyc$" "\.elc$"))


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
    (define-key map "\C-xk"     'my-recentf-kill-buffer)
    (define-key map "\C-x\C-q"  'my-recentf-exit-edit-mode)
    (setq my-recentf-edit-map map)))

;;
;; util
;;
(defun my-recentf-enumulate-file-path ()
  (let ((l (remove-duplicates (my-recentf-subst-tilda (my-recentf-truenames
                                                       (my-recentf-delete-not-exist
                                                        (my-recentf-delete-disused recentf-list))))
                              :from-end t :test #'string=)))
    (mapc #'(lambda (x) (insert x "\n")) l)
    (setq recentf-list l)))

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
      (buffer-substring-no-properties beg end))))

(defun my-recentf-find-file ()
    (let ((fn (my-recentf-get-current-line)))
      (if (file-exists-p fn)
          (progn
            ;; (my-recentf-update-list fn)
            (kill-buffer my-recentf-buf-name)
            (find-file fn))
          (message (concat "not found: " fn)))))

;; (defun my-recentf-update-list (e)
;;   (let ((l (reverse (set-exclusive-or recentf-list (list e) :test #'string=))))
;;     (setq recentf-list (push e l))))

(defun my-recentf-create-list ()
  "Recentf のバッファからリストを作る"
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))

(defun my-recentf-delete-empty-line (l)
  "空白行を削除"
  (delete-if #'(lambda (x) (string-match "^[ 　\t]*$" x)) l))


(defun my-recentf-delete-not-exist (l)
  "存在しないパスを削除"
    (delete-if #'(lambda (x) (and (not (my-recentf-ignore-p x)) (not (file-exists-p x)))) l))

(defun my-recentf-delete-disused (l)
  "不要なパスを削除"
  (flet ((disused-p (x)
                    (member-if #'(lambda (i) (string-match i x)) my-recentf-exclude-list)))
    (delete-if #'disused-p l)))

(defun my-recentf-truenames (l)
    (mapcar #'(lambda (x) (if (my-recentf-ignore-p x) x (file-truename x))) l))

(defun my-recentf-directory-trail-slash (l)
  (flet ((last-char (s) (string-to-char (substring s (1- (length s))))))
    (mapcar #'(lambda (x) (if (and (not (my-recentf-ignore-p x)) (file-directory-p x) (/= (last-char x) ?/)) (concat x "/") x)) l)))

(defun my-recentf-ignore-p (x)
  (member-if #'(lambda (i) (string-match i x)) my-recentf-ignore-list))

;; for windows
;; (defun my-recentf-upcase-drive-letter ()
;;   (let ((out   (mapcar #'upcase-drive-letter (my-recentf-create-list))))
;;     (delete-region (point-min) (point-max))
;;     (dolist (e out)
;;       (insert e "\n"))))

(defun my-recentf-subst-tilda (l)
  "e.g. /Users/john/readme.txt --> ~/readme.txt"
  (let ((directory-abbrev-alist `((,(concat "\\`" (getenv "HOME")) . "~"))))
    (mapcar #'(lambda (x) (abbreviate-file-name x)) l)))

;;
;; command
;;
(defun my-recentf-action-key-enter ()
  (interactive)
  (if buffer-read-only
      (my-recentf-find-file)
    (newline)))

(defun my-recentf-save ()
  (setq recentf-list (remove-duplicates (my-recentf-directory-trail-slash (my-recentf-truenames (my-recentf-subst-tilda (my-recentf-delete-not-exist
                                          (my-recentf-delete-empty-line (my-recentf-create-list))))))
                                        :from-end t :test #'string=)))

(defun my-recentf-kill-buffer ()
  (interactive)
  (kill-buffer my-recentf-buf-name)
  (set-window-configuration my-recentf-window-conf))

(defun my-recentf-enter-edit-mode ()
  (interactive)
  (setq buffer-read-only nil)
  (set-buffer-modified-p nil)
  (view-mode-exit t)
  (use-local-map my-recentf-edit-map)
  (message "entered to edit mode."))

(defun my-recentf-exit-edit-mode ()
  (interactive)
  (if (buffer-modified-p)
      (progn
        (my-recentf-save)
        (erase-buffer)
        (my-recentf-enumulate-file-path)
        (my-recentf-set-directory-face)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        ;; (setq buffer-read-only t)
        (message "exit from edit mode, saved."))
    (message "exit from edit mode."))
  (view-mode-enter)
  (use-local-map my-recentf-mode-map))
  

(defun my-recentf ()
  (interactive)
  (when (get-buffer my-recentf-buf-name)
    (kill-buffer my-recentf-buf-name))
  (setq my-recentf-window-conf (current-window-configuration))
  (set-buffer (get-buffer-create my-recentf-buf-name))
  (my-recentf-enumulate-file-path)
  (my-recentf-set-directory-face)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (view-mode-enter)
  (my-recentf-mode)
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
