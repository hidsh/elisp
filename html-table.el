;; html-table.el
;;
;; create html table from region of data.
;;
;; data is seperated by space or tab, like below.
;;
;; # name qty unit-price
;; 1 desk 1 30000
;; 2 table  1 50000
;; 3 chair 4 15000
;; 4 closet 1 80000
;;

(defvar html-table-header-location 'top
  "header location of the table. \'top , \'left or \'both")
;; (makunbound 'html-table-header-location)

(defvar html-table-css-string "table-layout: fixed"
  "css string of the table")

(defvar html-table-preview-browser "C:\\Program Files\\Mozilla Firefox\\firefox.exe"
  "path string to use as browser")
;; (makunbound 'html-table-preview-browser)

(defvar html-table-default-html-mode 'html-mode
  "mode-symbol of html-mode to use for default.")
;; (makunbound 'html-table-default-html-mode)

(defun html-table-get-contents-from-region (beg end)
  "return list of table-contents from region."
  (let ((cols '()))
    (save-excursion
      (goto-char beg) (beginning-of-line)
      (let (s)
	(while (< (point) end)
	  (setq s (buffer-substring-no-properties (point)
						  (save-excursion
						    (end-of-line)
						    (point))))
	  (unless (string= s "")
	    (setq cols (append cols (list (split-string s)))))
	  (forward-line 1))))
    cols))

(defun html-table-get-string-from-list (list)
  "return string of the result of create table from region."
  (let ((tag-tbl "table") (tag-th "th") (tag-tr "tr") (tag-td "td")
	(comment-beg "<!-- ### ") (comment-end " ### -->")
	(fmt-top '("<%s %s>" . "</%s>"))
	(fmt     '("<%s>" . "</%s>"))
	(style (format "style=\"%s\"" html-table-css-string))
	(ret ""))
    (let ((nline 0)
	  line tag)
      (setq ret (concat (format (car fmt-top) tag-tbl style) "\n"))
      (while list
	(setq line (car list))
	(let ((ncol 0)
	      (s ""))
	  (setq ret (concat ret s (format (car fmt) tag-tr) comment-beg
			    (format "LINE: %d" nline) comment-end "\n"))
	  (while line
	    (setq tag (cond ((and (eq html-table-header-location 'top)
				  (< nline 1)) tag-th)
			    ((and (eq html-table-header-location 'left)
				  (< ncol 1)) tag-th)
			    ((and (eq html-table-header-location 'both)
				  (or (< nline 1) (< ncol 1))) tag-th)
			    (t tag-td)))
	    (setq s (concat s (format (car fmt) tag) (car line)
			    (format (cdr fmt) tag) "\n"))
	    (setq line (cdr line))
	    (setq ncol (1+ ncol)))
	  (setq ret (concat ret s))
	  (setq ret (concat ret (format (cdr fmt) tag-tr) "\n")))
	(setq list (cdr list))
	(setq nline (1+ nline)))
      (setq ret (concat ret (format (cdr fmt) tag-tbl) "\n")))
    ret))

(defun html-table-preview ()
  "preview html using www browser."
  (let ((header "<html><body>\n") (footer "</body></html>\n")
	(name "html-table")
	(file-name (make-temp-name
		    (expand-file-name name temporary-file-directory)))
	(backup-inhibited t))
    (with-temp-file file-name
      (set-visited-file-name file-name)
      (insert header s footer)
      (save-buffer)
      (let ((w32-start-process-show-window t))
	(start-process name nil html-table-preview-browser
		       (concat "file://" file-name))))
    (sit-for 10)
    (delete-file file-name)))

(defun html-table (beg end)
  "create html table from region."
  (interactive "r")
  (let ((name "html-table"))
    (if (and beg end (> (- end beg) 1))
	(let* ((contents (html-table-get-contents-from-region beg end))
	       (s (html-table-get-string-from-list contents)))
	  (if s
	      (save-current-buffer
		(let ((buf (get-buffer name)))
		  (when buf (kill-buffer buf))
		  (setq buf (get-buffer-create name))
		  (switch-to-buffer-other-window buf t)
		  (funcall html-table-default-html-mode) ; mode set
		  (save-excursion (insert s))
		  (html-table-preview)))
	    (error "error occured.")))
      (error "invalid region."))))

;; html-table.el ends here
