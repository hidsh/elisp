;;
;; todays-save-log.el
;;
;; description:
;;   logging file path which is saved today, after saving
;;
;; command:
;;   todays-save-log-open-log-file
;;   todays-save-log  <-- from `after-save-hook'
;;
;; customize variable:
;;   todays-save-log-file-store-directory
;;   todays-save-log-exclude-list
;;

;;
;; variables and constants
;;

(defconst todays-save-log-file-store-directory "~/.log/"
  "directory path which is storing log file")

(defconst todays-save-log-exclude-list '("~/.recentf"
					 "~/.emacs-places")
  "list of file path want to be excluded")

(defconst todays-save-log-header-format
  (concat "#\n"
	  "# created by `todays-save-log.el'\n"
	  "#\n"
	  "# %s\n"
	  "#\n"
	  "# %d files, %s\n"
	  "#\n\n")
  "header string for log file")

;;
;; sub functions
;;

(defun todays-save-log-insert-header ()
  "insert header string for log-file."
  (save-excursion
    (let* ((name (file-name-nondirectory log-file))
	   (cnt (count-lines (progn (goto-char (point-min)))
			     (progn (goto-char (point-max)))))
	   (update (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
	   (s (format todays-save-log-header-format name cnt update)))
      (goto-char (point-min))
      (insert s))))

(defun todays-save-log-delete-header-lines ()
  "delete header lines which is started character `#'."
  (let ((beg (progn (goto-char (point-min)) (point)))
	end)
    (save-excursion
      (re-search-forward "^[^#]" nil t)
      (setq end (point)))
    (delete-region beg end)))


(defun todays-save-log-delete-old-item (path-str)
  "delete an old item from existing log-file, after inserted into temporaly buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward path-str nil t)
      (let ((beg (progn (beginning-of-line) (point)))
	    (end (progn (forward-line 1) (point))))
	(delete-region beg end)))))

(defun todays-save-log-get-file-name ()
  "return string which is today's log file name."
  (expand-file-name
   (concat todays-save-log-file-store-directory
	   (format-time-string "%y%m%d" (current-time))
	   ".txt")))

;;
;; main function
;;

(defun todays-save-log (path-str)
  "logging for saved files. this function is called from `after-save-hook'."
  (let* ((log-file (todays-save-log-get-file-name))
	 (excludes (append todays-save-log-exclude-list (list log-file))))
    (unless (member (expand-file-name path-str) excludes)
      (let* ((name-str (expand-file-name path-str))
	     (date-str (format-time-string "%H:%M:%S"
					   (nth 5 (file-attributes name-str))))
	     (s (format "%-70s%s\n" (abbreviate-file-name name-str) date-str)))
	(unless (file-directory-p todays-save-log-file-store-directory)
	  (make-directory todays-save-log-file-store-directory))
	(with-temp-file log-file
	  (when (file-exists-p log-file)
	    (insert-file-contents log-file)
	    (todays-save-log-delete-header-lines)
	    (todays-save-log-delete-old-item (abbreviate-file-name name-str)))
	  (insert s)
	  (todays-save-log-insert-header))))))

;;
;; commands
;;

(defun todays-save-log-open-log-file ()
  "open today's log file."
  (interactive)
  (let ((fn (todays-save-log-get-file-name)))
    (if (file-exists-p (expand-file-name fn))
	(find-file-read-only (expand-file-name fn))
      (message "can not found today's log. \"%s\"" (abbreviate-file-name fn)))))

;;
;; add-hooks and defadvices
;;

(add-hook 'after-save-hook
	  (lambda ()
	    (let ((fn (abbreviate-file-name
		       (expand-file-name (buffer-file-name)))))
	      (todays-save-log fn))))

;;
;; end
;;

(provide 'todays-save-log)

;; todays-save-log.el ends here
