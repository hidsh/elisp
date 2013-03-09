;;
;; rif.el
;; rif (Relax, even IF thousand of ifdefed directives mess you up)
;;
;; command:
;;   rif-show-symbol-value-at-point
;;   rif-create-symbol-list
;;   rif-open-symbol-file

(require 'hideif)

;;
;; user variables
;;
(defvar rif-sym-list-file-name ".rif-sym-list"
  "")

(defvar rif-auto-apply nil
  "")


;;
;; internal variables and constance
;;
(defvar rif-sym-list '()
  "alist of symbol-list \(\(name . value\)\(name . value\)...\)")

(defvar rif-sym-list-directory-try-to-load-from nil
  "directory string which is that user tried to load rif-sym-list from.")

(defvar rif-sym-list-path nil
  "path string of rif\'s symbol file which is cached loaded it even at once.")

;; internal variables and constance
(defconst rif-sym-list-header
  (concat (format ";; %s\n" rif-sym-list-file-name)
	  ";;\n"
	  ";; rif\(Relax, even IF thousand of ifdefed directives mess you up\)\n"
	  ";; treats this file as symbol-list-file.\n"
	  ";;\n\n"
	  "\(setq rif-sym-list \'\(\n"))

(defconst rif-sym-list-footer
  (concat "\)\)\n\n"
	  (format ";; %s ends here.\n" rif-sym-list-file-name)))

;;
;; functions
;;
(defun rif-read-search-word ()
  (let (word input (wd nil) (init nil) (pt 1))
    ;; get a word near the point as search word
    (setq wd (thing-at-point 'symbol))
    (if (stringp wd)
	(progn
	  (set-text-properties 0 (length wd) nil wd)
	  ;; put point to the end of default word
	  (setq pt (1+ (length wd))))
      (setq wd ""))
    (setq init (cons wd pt))
    (setq input
	  (read-from-minibuffer "search-symbol: " init))
    (list input)))

(defun rif-search-symbol-value (name)
  "Return symbol's value which is result of search key in symbol-list."
    (let* ((al (assq (intern name) rif-sym-list))
	   (val (cdr al)))
      (cond ((and val (symbolp val))
	     (setq result (rif-search-symbol-value (symbol-name val)))
	     (let ((wd "="))
	       (set-text-properties 0 (length wd)
				    '(face font-lock-doc-face) wd)
	       (cons (car result) (concat name wd (cdr result)))))
	    (t
	     (let ((wd "=>"))
	       (set-text-properties 0 (length wd)
				    '(face font-lock-doc-face) wd)
	       (cons val (concat name wd)))))))

(defun rif-show-symbol-value-at-point (word)
  "show symbol\'s value in minibuffer."
  (interactive (rif-read-search-word))
  (let* ((result (rif-search-symbol-value word))
	 (value (car result))
	 (route (cdr result)))
    (message (cond ((numberp value)
		    (format "%s%d" route value))
		   ((stringp value)
		    (format "%s\"%s\"" route value))
		   (t
		    (format "%snot found" route))))))

;; not need to input symbol's name
;;
;; (defun rif-show-symbol-value-at-point ()
;;   "show symbol\'s value in minibuffer."
;;   (interactive)
;;   (let ((word (thing-at-point 'symbol)))
;;     (if (stringp word)
;; 	(set-text-properties 0 (length word) nil word)
;;       (setq word ""))
;;     (let* ((result (rif-search-symbol-value word))
;; 	   (value (car result))
;; 	   (route (cdr result)))
;;       (message (cond ((numberp value)
;; 		      (format "%s%d" route value))
;; 		     ((stringp value)
;; 		      (format "%s\"%s\"" route value))
;; 		     (t
;; 		      (format "%snot found" route)))))))




(defun rif-get-all-symbols-in-current-buffer ()
  "Return alist ((name . (beg . end)) (name . (beg . end)) ...) of
enumeration which is all symbols in the current buffer."
  (let ((sym-list '())
	ret)
    (save-excursion
      (goto-char (point-min))
      (while (setq ret (rif-get-next-if-symbol))
	(unless (string= (cdr ret) "")	; symbol-name
	  (setq sym-list (append sym-list (list ret))))
	(got-char (cdr (car (car ret))))))
    sym-list))

(defun rif-get-symbol-lines-from-binary-elf (path)
  "Return string consists symbol-list from elf-dwarf binary. \(name . \"value\"\)"
  (if (file-exists-p path)
      (let* ((bin-path (expand-file-name path))
	     (awk-path "~/elisp/rif_elf.awk")
	     (cmd (format "readelf --debug-dump=macro %s | gawk -f %s | sort -u"
			  bin-path awk-path)))
	(shell-command-to-string cmd))
    nil))


(defun rif-create-symbol-list (bin)
  "get symbol-list from binary object such as elf-dwarf format.
format of the list: \(\(name . \"val\"\) \(name . \"val\"\) ...\)"
  (interactive "fbin-obj:")
  (if (and bin
	   (file-exists-p bin)
	   (not (file-directory-p bin))
	   (file-regular-p bin)
	   ;; todo: $ file bin --> (and "ELF" "not stripped")
	   )
      (let ((s (prog2
		   (message "processing...")
		   (unwind-protect
		       (rif-get-symbol-lines-from-binary-elf bin)
		     nil)
		 (message (concat (current-message) "done."))
		 (sit-for 1))))
	(if (and s (not (string= s "")))
	    (if (interactive-p)
		(progn
		  (setq s (with-temp-buffer
			    (insert rif-sym-list-header
				    s
				    rif-sym-list-footer)
			    (eval-buffer (buffer-name))
			    (buffer-string)))
		  (when (y-or-n-p "save symbol file?")
		    (let* ((dir (if rif-sym-list-directory-try-to-load-from
				    rif-sym-list-directory-try-to-load-from
				  (file-name-directory bin)))
			   (path (concat (expand-file-name dir)
					 rif-sym-list-file-name)))
		      (with-temp-file path
			(insert s))
		      (message "saved to %s." (abbreviate-file-name path)))
		    (setq rif-sym-list-directory-try-to-load-from nil)))
	      ;; for debug
	      (insert "\n;; ------------------\n"
		      "\(setq l '\(\n"
		      s
		      "\)\)\n"))
	  (error "can not get symbol-list.")))
    (error "no binary object.")))


(defun get-directory-from-current-buffer ()
  "return directory string of the file which is visited as current buffer.
otherwise return nil if current buffer is not visited."
  (let ((curr-path (expand-file-name (buffer-file-name))))
    (if curr-path
	(abbreviate-file-name (file-name-directory curr-path))
      nil)))

(defun rif-read-directory ()
  (let ((dir (get-directory-from-current-buffer)))
    (setq dir
          (if (and (boundp 'running-xemacs) running-xemacs)
              (read-directory-name "Directory: " dir)
            (read-file-name "Directory: " nil nil t)))
    (if (and (file-exists-p dir)
             (file-directory-p  dir))
        (setq dir (file-name-as-directory dir))
      (setq dir (file-name-as-directory (file-name-directory dir)))
      (unless (and (file-exists-p dir)
		   (file-directory-p  dir))
        (error (format "No such directory \"%s\"" dir))
        (sleep-for 1)
        (setq dir nil)))
    (list dir)))


(defun rif-load-symbol-list-sub (dir)
  "load symbol-list from symbol-file which is created by \`rif-create-symbol-list\'"
  (let ((path (concat (expand-file-name dir) rif-sym-list-file-name))
	(ret '()))
    (if (file-exists-p path)
	(if (load path t nil t)
	    (setq ret (cons 'succeeded path))
	  (setq ret (cons 'err-can-not-load (abbreviate-file-name path))))
      (setq ret (cons 'err-not-found (abbreviate-file-name path)))
      (sit-for 1)
      (if (y-or-n-p "create symbol-list from binary object?")
	  (progn
	    (call-interactively 'rif-create-symbol-list)
	    (rif-load-symbol-list-sub dir))
	(setq ret (cons 'quit ""))))
    ret))

(defun rif-load-symbol-list (dir)
  "Load symbol list file from dir.
Then return t loading had succeeded, otherwise return nil."
  (interactive (rif-read-directory))
  (if (file-exists-p (expand-file-name dir))
      (progn
	(setq rif-sym-list-directory-try-to-load-from dir)
	(let* ((ret-list (rif-load-symbol-list-sub dir))
	       (ret-code (car ret-list))
	       (s (cdr ret-list))
	       (ret (if (eq ret-code 'succeeded) t nil)))
	  (cond ((and (eq ret-code 'succeeded) rif-sym-list)
		 ;; 	   (rif-optimize-symbol-list))
		 (setq rif-sym-list-path s))
		((eq ret-code 'err-can-not-load)
		 (error "can not load symbols from symbol-list file."))
		((eq ret-code 'err-not-found)
		 (error "not found symbol-list file. \"%s\"" (cdr result)))
		((eq ret-code 'quit)
		 (message "quit."))
		(t (error "unknown error.")))))
    (error "directory does not exists. \"%s\"" dir)))


(defun rif-apply-symbol-list ()
  "load and apply symbol-list to hide-ifdef-env which is part of hide-ifdef-mode.
Return t if succeeded to apply, otherwise return nil."
  (if rif-sym-list-path
      (rif-load-symbol-list-sub (file-name-directory rif-sym-list-path))
    (when (y-or-n-p "load from symbol using rif?")
	  (when (call-interactively' rif-load-symbol-list)
	      (rif-apply-symbol-list))))
  (if rif-sym-list
      (progn
	(message "apply...")
	(setq hide-ifdef-env rif-sym-list)
	(message (concat (current-message) "done."))
	t)
    nil))


;; main procedure
(defun rif ()
  (interactive)
  (if hide-ifdef-env
      (message "already loaded symbol list.")
    (when (rif-apply-symbol-list)
      (show-ifdefs)
      (unwind-protect
	  (hide-ifdefs)
	(show-ifdefs)
	(setq max-lisp-eval-depth (round (* max-lisp-eval-depth 1.5)))
	(message "shortage of `max-lisp-eval-depth'. increased it. try again.")))))

(defun rif-open-symbol-file ()
  (interactive)
  (if rif-sym-list-path
      (progn
	(find-file-read-only rif-sym-list-path)
	(emacs-lisp-mode))
    (message "no currently symbol list file.")))


;;
;; hooks and advices
;;
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when rif-auto-apply
	      (rif))))

;;;
;;; end
;;;
(provide 'rif)

;;; rif.el ends here
