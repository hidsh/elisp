(require 'color-moccur)

(defun find-file-find-in-dir (dir inputs)
  "fully based on \'moccur-grep-find\'"
  (interactive
   (list (moccur-grep-read-directory)
         (moccur-grep-read-regexp moccur-grep-default-mask)))
  (moccur-setup)
  (setq moccur-last-command 'moccur-grep-find)

  (let (regexps
        mask (files nil)
        ;;(default-directory dir)
        )
    (setq regexps
          (mapconcat 'concat
                     (if (= 1 (length inputs))
                         inputs
                       (reverse (cdr (reverse inputs))))
                     " "))
    (setq mask
          (if (= 1 (length inputs))
              "."
            (car (reverse inputs))))
    (message "Listing files...")
    (cond
     ((listp dir)
      (while dir
        (cond
         ((file-directory-p (car dir))
          (setq files (append
                       (reverse (moccur-grep-find-subdir (car dir) mask))
                       files)))
         (t
          (setq files (cons
                       (car dir)
                       files))))
        (setq dir (cdr dir))))
     (t
      (setq files (reverse (moccur-grep-find-subdir dir mask)))))
    (message "Listing files done!")
    (find-file-find-in-dir-moccur-search-files regexps files)))

(defun find-file-find-in-dir-moccur-search-files (regexp files)
  "fully based on \'moccur-search-files\'"
  ;; initialize
  (moccur-search-files-init regexp files)

  (save-excursion
    (setq moccur-mocur-buffer (generate-new-buffer "*Moccur*"))
    (set-buffer moccur-mocur-buffer)
    ;; (moccur-insert-heading moccur-regexp-input)

    ;; search all buffers
    ;; (moccur-search-all-files files)
    (find-file-find-in-dir-insert-file-path regexp files)
    (message "Searching done!")
    (if (> moccur-matches 0)
        (progn
          (set-buffer moccur-mocur-buffer)
          (delete-other-windows)
          (moccur-grep-mode)
          ;; highlight Moccur buffer
          (moccur-buffer-color)
          (setq buffer-undo-list nil)

          ;; move cursor to the first matching text
          (set-buffer moccur-mocur-buffer)
          ;;(setq moccur-view-other-window nil)

          (pop-to-buffer moccur-mocur-buffer)
          (goto-char (point-min))

          (make-local-variable 'moccur-xdoc2txt-buffers)
          (setq moccur-xdoc2txt-buffers nil)

          (message "%d matches" moccur-matches)
          (local-set-key "\C-m" 'find-file-find-in-dir-goto)
          t)
      (message "no matches")
      (moccur-kill-buffer t)
      (moccur-remove-overlays-on-all-buffers)
      nil)))

(defun find-file-find-in-dir-insert-file-path (regexp files)
  (setq moccur-matches 0)
  (dolist (f files)
    (when (string-match regexp f)
      ;; (insert (file-relative-name f default-directory) "\n")
      (insert f "\n")
      (setq moccur-matches (+ moccur-matches 1)))))

(defun find-file-find-in-dir-goto ()
  (interactive)
  (let ((buf (current-buffer)))
    (find-file (buffer-substring (line-beginning-position) (line-end-position)))
    (bury-buffer buf)))


(global-set-key "\M-i" 'find-file-find-in-dir)

(provide 'find-file-find)

