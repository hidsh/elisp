;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-
;;;;
;;;;    discrete elisp
;;;;
;;;;	Filename: discrete.el
;;;;	Last modified: Mon Mar 04 2013 15:45:58 JST
;;;;
;;;;	based: $Id: discrete.el,v 1.51 2006/02/16 05:13:34 gnrr Exp gnrr $
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		    I WROTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ common functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun minibuffer-p (&optional window)
  (unless window (setq window (selected-window)))
  (eq window (minibuffer-window (window-frame window))))

(defun get-max-string-length-in-list (list)
  "return maximum value of string length in list"
  (let ((max 0))
    (while list
      (let ((len (length (car list))))
	(setq max (if (< max len) len max)))
      (setq list (cdr list)))
    max))

(defun narrowing-p ()
  "return t if current buffer is narrowed, otherwise return nil."
  (if (string-match "<[0-9]+[ ]*-[ ]*[0-9]+>" (what-cursor-position))
      t
    nil))

(defun show-face-at-point ()
  "Show faces of character in minibuffer."
  (interactive)
  (let* ((s (buffer-substring (point) (1+ (point))))
	 (faces (get-text-property 0 'face s))
	 msg)
    (if faces
	(progn
	  (unless (listp faces)
	    (setq faces (list faces)))
	  (while faces
	    (setq msg (concat msg " \'" (symbol-name (car faces))))
	    (setq faces (cdr faces)))
	  (setq msg (substring msg 1 (length msg))))
      (setq msg "no faces"))
    (message msg)))

(defun get-word-in-string (s &optional offset)
  "Return word string in string."
  (let* ((len (length s))
	 (ofs (if (and offset (> offset 0)) offset 0))
	 (n ofs)
	 (wd (substring s ofs len))
	 ch)
    (while (< n len)
      (setq ch (elt s n))
      (if (or (= ch ?\ ) (= ch ?\t) (= ch ?/))
	  (progn
	    (setq wd (substring s ofs n))
	    (setq n len))
	(setq n (1+ n))))
    wd))

(defun remove-heading-comment-character (s)
  "Return string which is removed heading comment characters.
otherwise return nil,if whole line is consisted of comment characters."
  (let ((len (length s))
	(n 0)
	(nyet t)
	faces)
    (while (and nyet (< n len))
	(setq faces (get-text-property n 'face s))
	(unless (listp faces)
	  (setq faces (list faces)))
	(if (or (memq 'font-lock-comment-face faces)
		(memq 'font-lock-comment-delimiter-face faces))
	    (setq n (1+ n))
	  (setq nyet nil)))
    (if nyet
	nil
      (substring s n len))))

(defun string-comment-p (s)
  "Return t when string is comment, otherwise return nil."
  (when s
    (let ((props (get-text-property 0 'face s)))
      (if (or (eq props 'font-lock-comment-face)
	      (eq props 'font-lock-comment-delimiter-face))
	  t nil))))

(defun pt ()
  "represent point value at point."
  (interactive)
  (message "point :%d" (point)))

(defun remove-heading-spaces (s)
  "Return string which is removed heading spaces in string."
  (let ((nyet t)
        (n 0)
        (len (length s))
        beg)
    (while (and nyet (< n len))
      (if (= 32 (char-syntax (elt s n))) ; 32: space class
          (setq n (1+ n))
        (setq nyet nil)))
    (substring s n len)))

(defun get-line-string (num &optional no-prop)
  "Return string of the line which you specified by num. when no-prop is non-nil,
return string with property, otherwise nil return it without property."
  (let (beg)
    (save-excursion
      (goto-char (point-min))
      (if (> (forward-line (1- num)) 0)
	  nil
	(setq beg (point))
	(if no-prop
	    (buffer-substring-no-properties beg (progn (end-of-line) (point)))
	  (buffer-substring beg (progn (end-of-line) (point))))))))

(defun my-replace-match-string (str search replace)
  (let ((s str)
	(pos nil)
	(len (length replace)))
    (while (setq pos (string-match search s pos))
      (setq s (replace-match replace t t s))
      (setq pos (+ pos len)))
    s))

(defun get-variable-name (var)
  "Return string of variable-name."
    (format "%s" (eval var)))

(defun get-directory-from-current-buffer ()
  "return directory of the file which is visited as current buffer
otherwise return nil if current buffer is not visited."
  (let ((curr-path (buffer-file-name))
	dir)
    (if curr-path
	(setq dir (abbreviate-file-name (file-name-directory curr-path)))
      nil)))

(defun get-quote-removed-string (s)
  "Return string which removed quotation characters \(\'\) and
double quotation characters \(\"\) from given string."
  (if (string= s "")
      ""
    (let ((len (length s))
	  (n 0)
	  (new "")
	  c)
      (while (< n len)
	(setq c (elt s n))
	(unless (or (= c ?\') (= c ?\"))
	  (setq new (concat new (string c))))
	(setq n (1+ n)))
      new)))

(defun get-space-removed-string (s)
  "Return string which removed spa ce characters \" \" from given string."
  (if (string= s "")
      ""
    (let ((len (length s))
	  (n 0)
	  (new "")
	  c)
      (while (< n len)
	(setq c (elt s n))
	(unless (= c ? )
	  (setq new (concat new (string c))))
	(setq n (1+ n)))
      new)))

(defun get-beg-end-spaces-removed-string (s)
  "Return string which removed spaces at begin/end of given string."
  (if (string= s "")
      ""
    (let* ((len (length s))
	   (spc ?\ )
	   n c nyet beg end)
      (setq nyet t)
      (setq n 0)
      ;; from begin of string
      (while (and nyet (< n len))
	(setq beg n)
	(setq c (elt s n))
	(unless (= c spc)
	  (setq nyet nil))
	(setq n (1+ n)))

      (setq nyet t)
      (setq n (1- len))
      ;; from end of string
      (while (and nyet (> n -1))
	(setq end n)
	(setq c (elt s n))
	(unless (= c spc)
	  (setq nyet nil))
	(setq n (1- n)))
      (substring s beg (1+ end)))))

(defun buffer-exists-p (buffer-name)
  "Return t when buffer exists which is named `buffer-name', ohterwise return nil."
  (if (member buffer-name
	      (mapcar (function buffer-name) (buffer-list)))
      t
    nil))

(defun find-buffers-by-file-name (file-name)
  "Return buffer when buffer exists which has visiting file name, ohterwise return nil."
  (if (member file-name
	      (mapcar (function buffer-file-name) (buffer-list)))
      t
    nil))


(defun get-current-line-number (&optional pt)
  "Return current line number (starting from 1)"
  (unless pt (setq pt (point)))
  (let (start)
    (save-excursion
	(goto-char (point-min))
	(forward-line 0)
	(setq start (point))
	(goto-char pt)
	(forward-line 0)
	(1+ (count-lines start (point))))))

(defun get-pt-beginning-of-line (&optional pt)
  "Return point value of beginning-of-line."
  (unless pt (setq pt (point)))
  (save-excursion
    (beginning-of-line)
    (point)))

(defun get-pt-end-of-line (&optional pt)
  "Return point value of end-of-line."
  (unless pt (setq pt (point)))
  (save-excursion
    (end-of-line)
    (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-switch-to-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key "\C-xb" 'my-switch-to-buffer)
;; (global-set-key "\C-xb" 'electric-buffer-list)

;; (add-hook 'electric-buffer-menu-mode-hook
;;   '(lambda ()
;; 	 (view-mode-enter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ py-index-search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun py-index-search (topic)
  "like `elisp-index-search'"
  (interactive (let ((atpt (thing-at-point 'symbol)))
		 (unless atpt
		   (setq atpt ""))
		 (list (read-no-blanks-input "Subject to look up: " atpt))))
  (info-other-window "python-lib-jp.info")
  (Info-index topic))

;;   (let (result-str)
;;     (setq result-str (Info-index topic))
;;     (when (string-match "No .+ in index" result-str)
;;       (Info-exit))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-elisp-index-search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-elisp-index-search (topic)
  "replace `elisp-index-search'"
  (interactive (let ((atpt (thing-at-point 'symbol)))
		 (unless atpt
		   (setq atpt ""))
		 (list (read-no-blanks-input "Subject to look up: " atpt))))
  (info-other-window "elisp")
  (Info-index topic))

;; replace
(defalias 'elisp-index-search 'my-elisp-index-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-copy-buffer-file-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-copy-buffer-file-name ()
  "copy buffer-file-name to kill-ring."
  (interactive)
  (let ((fn (unwind-protect
		(buffer-file-name)
	      nil)))
    (if fn
	(let ((f (abbreviate-file-name (expand-file-name fn))))
	  (kill-new f)
	  (message "copied: \"%s\"" f))
      (message "no file name"))))

(global-set-key "\C-xf" 'my-copy-buffer-file-name)

;;
;; ;;;@@ copy-buffer-file-name
;;
;; (defun copy-buffer-file-name ()
;;   (interactive)
;;   (let ((str (buffer-file-name)))
;;     (if str
;; 	(progn
;; 	  (setq str (file-name-nondirectory (buffer-file-name)))
;; 	  (kill-new str)			;copy to kill-ring
;; 	  (message (concat "copied \"" str "\"")))
;;       (message (concat "no filename")))))

;; (global-set-key "\C-c\C-v" 'copy-buffer-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ message-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun message-buffer ()
  "Open `*Messages*' buffer."
  (interactive)
  (let ((buf "*Messages*"))
    (switch-to-buffer-other-window buf t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-eval-depth-increase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-eval-depth-increase ()
  "increase `max-lisp-eval-depth' for large recursive calling."
  (interactive)
  (setq max-lisp-eval-depth (round (* max-lisp-eval-depth 1.5)))
  (message "max-lisp-eval-depth => %d" max-lisp-eval-depth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-customized backward-word, forward-word, backward-kill-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-backward-word ()
  (interactive)
  (if (bolp)
	 (forward-char -1)
    (let ((pt (save-excursion (forward-word -1) (point))))
      (goto-char (max (get-pt-beginning-of-line) pt)))))

(defun my-forward-word ()
  (interactive)
  (if (eolp)
	 (forward-char 1)
    (let ((pt (save-excursion (forward-word 1) (point))))
      (goto-char (min (get-pt-end-of-line) pt)))))

(defun my-backward-kill-word ()
  (interactive)
  (if (bolp)
	 (delete-backward-char 1)
    (let ((pt (save-excursion (forward-word -1) (point))))
      (delete-region (point) (max (get-pt-beginning-of-line) pt)))))

(global-set-key "\M-b" 'my-backward-word)
(global-set-key "\M-f" 'my-forward-word)
(global-set-key "\M-h" 'my-backward-kill-word)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;@@ nippo
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar nippo-dir "~/biz/nippo/")
;; (defvar work-end-time '(18 . 10)
;;   "hour . minute")

;; (defvar work-end-time-margin 20
;;   "minute")

;; (defun nippo-get-the-date-list (time-list arg)
;;   (let ((ty (nth 5 time-list))
;; 	(tm (nth 4 time-list))
;; 	(td (nth 3 time-list))
;; 	(not-yet t))
;;     (setq td (+ td arg))
;;     (list (nth 0 time-list) (nth 1 time-list) (nth 2 time-list) td tm ty)))

;; (defun nippo-get-proper-file-name (time arg &optional no-today)
;;   (let* ((ext ".txt")
;; 	 (tl (decode-time time))
;; 	 (file-name-today (concat (format-time-string "%y%m%d" time) ext))
;; 	 (file-name file-name-today)
;; 	 (path nil)
;; 	 (is-today nil)
;; 	 (not-yet t)
;; 	 (no-err t))
;;     (while (and not-yet no-err)
;;       (setq path (concat (directory-file-name (expand-file-name nippo-dir)) "/"
;; 			 file-name))
;;       (when (file-exists-p path)
;; 	(setq is-today (if (string= file-name file-name-today) t nil))
;; 	(if no-today
;; 	    (unless is-today
;; 	      (setq not-yet nil))
;; 	  (setq not-yet nil)))
;;       (when not-yet
;; 	(let ((pl (nippo-get-the-date-list tl arg))
;; 	      (pt nil))
;; 	  (if pl
;; 	      (progn
;; 		(setq pt (encode-time (nth 0 pl) (nth 1 pl) (nth 2 pl) (nth 3 pl) (nth 4 pl) (nth 5 pl)))
;; 		(setq file-name (concat (format-time-string "%y%m%d" pt) ext))
;; 		(setq tl pl))
;; 	    (setq no-err nil)))))
;;     (if no-err
;; 	;; found
;; 	(list path is-today)
;;       ;; error (not found)
;;       nil)))

;; (defun nippo-get-todays-file-name (time)
;;   (let* ((ext ".txt"))
;;     (concat (format-time-string "%y%m%d" time) ext)))

;; (defun nippo-get-work-end-time (ct)
;;   (let* ((l (decode-time ct))
;; 	 (cm (nth 1 l))
;; 	 (ch (nth 2 l))
;; 	 (sec (+ (* ch 3600) (* cm 60)))
;; 	 (wh (car work-end-time))
;; 	 (wm (cdr work-end-time)))
;;     (if (< sec (+ (* wh 3600) (* wm 60)))
;; 	(cons wh wm)
;;       (if (< cm (- 60 work-end-time-margin))
;; 	  (cons ch (* (/ (+ cm work-end-time-margin) 10) 10))
;; 	(cons (1+ ch) 0)))))

;; (defun nippo-open-post-process-exists (ct)
;;   ;; time
;;   (goto-char (point-min))
;;   (re-search-forward "- [0-9][0-9]:[0-9][0-9]$")
;;   (let ((we (nippo-get-work-end-time ct)))
;;     (replace-match (format "- %02d:%02d" (car we) (cdr we))))

;;   (set-window-start (selected-window) 1))

;; (defun nippo-open-post-process-new (ct)
;;   (goto-char (point-min))
;;   ;; date
;;   (re-search-forward "[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] \\(.?\\)$")
;;   (replace-match (format-time-string "%Y/%m/%d (%a)" ct))

;;   ;; time
;;   (re-search-forward "- [0-9][0-9]:[0-9][0-9]$")
;;   (let ((we (nippo-get-work-end-time ct)))
;;     (replace-match (format "- %02d:%02d" (car we) (cdr we))))

;;   ;; next day or next week
;;   (goto-char (point-max))
;;   (re-search-backward "^$B!Z(B.*$B$NM=Dj![(B$")
;;   (let* ((fri 5)
;; 	 (s (if (< (string-to-number (format-time-string "%w" ct)) fri)
;; 		"$B!ZL@F|$NM=Dj![(B"
;; 	      "$B!ZMh=5$NM=Dj![(B")))
;;     (replace-match s))

;;   (set-window-start (selected-window) 1))


;; (defun nippo ()
;;   (interactive)
;;   (let* ((ct (current-time))
;; 	 (f (nippo-get-proper-file-name ct -1 nil))) ;include today's
;;     (if f
;; 	(let ((file-name (nth 0 f)))
;; 	  (find-file file-name)
;; 	  (if (nth 1 f) ;; today's file exist?
;; 	      (nippo-open-post-process-exists ct)
;; 	    (setq path (concat (directory-file-name (expand-file-name nippo-dir)) "/"
;; 			       (nippo-get-todays-file-name ct)))
;; 	    (write-file path)
;; 	    (nippo-open-post-process-new ct)
;; 	    (write-file path)
;; 	    (message "today's nippo is new created."))
;; 	  (set-buffer-modified-p nil))
;;       (message "can not open nippo."))))

;; (defun nippo-get-the-days-nippo-file-name-relative (dir-list curr-file x)
;;   ""
;;   (if curr-file
;;       (let ((n 0)
;; 	    (found nil))
;; 	(while (and (not found) (< n (length dir-list)))
;; 	  (if (string= (nth n dir-list) curr-file)
;; 	      (setq found t)
;; 	    (setq n (1+ n))))
;; 	(if found
;; 	    (if (< n (length dir-list))
;; 		(progn
;; 		  (setq n (+ n x))
;; 		  (if (>= n 0)
;; 		      (nth (+ n x) dir-list)
;; 		    nil))
;; 	      nil)
;; 	  nil))
;;     (nth (1- (length dir-list)) dir-list)))

;; (defun nippo-open-the-days-nippo-or-memo (arg)
;;   (let* ((cf (expand-file-name (buffer-file-name)))
;; 	 (curr (if (file-exists-p cf) cf nil))
;; 	 (dir (file-name-directory cf))
;; 	 (l (directory-files dir t "[.]txt$"))
;; 	 (f (nippo-get-the-days-nippo-file-name-relative l curr arg)))
;;     (when (and f (not (string= cf f)))
;;       (kill-buffer (current-buffer))
;;       (find-file-read-only f))
;;     f))

;; (defun nippo-open-previous ()
;;   (interactive)
;;   (unless (nippo-open-the-days-nippo-or-memo -1)
;;     (message "can not found previous.")))

;; (defun nippo-open-next ()
;;   (interactive)
;;   (unless (nippo-open-the-days-nippo-or-memo 1)
;;     (message "can not found next.")))

;; (defun nippo-get-the-days-nippo-file-name-absolute (dir-list date)
;;   (let ((ext ".txt")
;; 	(n 0)
;; 	(found nil)
;; 	(nyet t)
;; 	path)
;;     (while (and nyet (< n (length dir-list)))
;;       (setq path (nth n dir-list))
;;       (cond
;;        ((string= path (concat (directory-file-name (expand-file-name nippo-dir))
;; 			      "/"
;; 			      date
;; 			      ext))
;; 	(setq found t)
;; 	(setq nyet nil))
;;        ((< (string-to-number date) (string-to-number
;; 				    (file-name-sans-extension
;; 				     (file-name-nondirectory path))))
;; 	(setq nyet nil))
;;        (t (setq n (1+ n)))))
;;     (if nyet
;; 	(cons "" nil)
;;       (cons path found))))

;; (defun nippo-open-go (arg)
;;   (interactive "sdate:")
;;   (let ((l (directory-files (expand-file-name nippo-dir) t "[.]txt$"))
;; 	(f nil))
;;   (cond
;;    ((or (string= arg "b") (string= arg "beg") (string= arg "s") (string= arg "start"))
;;     (setq f (cons (car l) t)))		  ; (filename . t)
;;    ((or (string= arg "") (string= arg "e") (string= arg "end"))
;;     (setq f (cons (nth (1- (length l)) l) t))) ; (filename . t)
;;    (t
;;     (cond
;;      ((string-match "^[0-9][0-9][0-9][0-9][0-9][0-9]$" arg) ;yymmdd
;;       (setq f (nippo-get-the-days-nippo-file-name-absolute l arg)))
;;      ((string-match "^[0-9][0-9][0-9][0-9]$" arg)		  ;mmdd
;;       (let ((yy (format-time-string "%y" (current-time))))
;; 	(setq f (nippo-get-the-days-nippo-file-name-absolute l
;; 							     (concat yy arg)))))
;;      ((string-match "^[0-9][0-9]$" arg)			  ;dd
;;       (let ((yymm (format-time-string "%y%m" (current-time))))
;; 	(setq f (nippo-get-the-days-nippo-file-name-absolute l
;; 							     (concat yymm arg)))))
;;      ((string-match "^[0-9]$" arg)			  ;d
;;       (let ((yymm (format-time-string "%y%m" (current-time))))
;; 	(setq f (nippo-get-the-days-nippo-file-name-absolute l
;; 							     (concat yymm "0" arg)))))
;;      (t (message "invalid format: s(start), b(begin) | e(end) |d | dd | mmdd | yymmdd")))))
;;   (when f
;;     (cond
;;      ((string= (car f) "")
;;       (message "not found spesified nippo."))
;;      (t
;;       (unless (string= (buffer-file-name (current-buffer)) (car f))
;; 	(kill-buffer (current-buffer))
;; 	(find-file-read-only (car f))
;; 	(unless (cdr f)
;; 	  (message "not found. opened nearest date's."))))))))


;; (define-key text-mode-map "\C-x\C-p" 'nippo-open-previous)
;; (define-key text-mode-map "\C-x\C-n" 'nippo-open-next)
;; (define-key text-mode-map "\C-x\C-g" 'nippo-open-go)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ gtags-find-file-from-memo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar gtags-find-file-from-memo-tag-file-directory "~/proj/src/"
  "path to directory which contains \"GTAGS\".")

(defun gtags-find-file-from-memo ()
  (if (functionp 'gtags-goto-tag)
      (let (result file line)
	(save-excursion
	  (let ((lmt (progn (end-of-line) (point))))
	    (beginning-of-line)
	    (setq result (re-search-forward "@[^:].*" lmt t))))
	(let ((l (split-string (match-string-no-properties 0) "[@:]")))
	  (if (and result l (listp l) (> (length l) 2))
	      (progn
		(setq file (nth 1 l))
		(setq line (string-to-number (substring (nth 2 l) 1)))
		(let* ((gtags-rootdir (expand-file-name
				      gtags-find-file-from-memo-tag-file-directory))
		       (default-directory gtags-rootdir))
		  (gtags-push-context)
		  (gtags-goto-tag file "P")
		  (goto-char (point-min))
		  (goto-line line)))
	    (error "invalid line."))))
    (error "gtags is not loaded yet.")))

(defun open-file-at-point ()
  "return file name string which can be open. otherwise return nil if it failed."
  (let ((s (thing-at-point 'filename)))
    (if s
	(let ((path (expand-file-name s)))
	  (if (and (file-exists-p path) (file-regular-p path))
	      (progn
		(find-file (expand-file-name path))
		path)
	    ;; invalid file name
	    nil))
      ;; can not found file name
      nil)))

(defun my-open-file-at-point ()
  (interactive)
  (if (not (open-file-at-point))
      (gtags-find-file-from-memo)))


;; (define-key text-mode-map "\M-j" 'my-open-file-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ daily-memo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar daily-memo-file-directory "~/memo/")
(defvar daily-memo-prev-buffer nil)
(defvar daily-memo-comment-string "***** ")

;; get function name
(defun daily-memo-get-pos-eol (pt)
  (goto-char pt)
  (let (pos)
    (save-excursion
      (end-of-line)
      (setq pos (point)))
    pos))

(defun daily-memo-get-function-name-pos-begin (beg)
  (let ((faces ())
	(not-yet t)
	(eol (daily-memo-get-pos-eol beg))
	(pos (1- beg)))
    (while not-yet
      (setq pos (1+ pos))
      (if (= pos eol)
	  (progn
	    (setq pos nil)
	    (setq not-yet nil))
	(setq faces (get-text-property pos 'face))
	(unless (listp faces)
	  (setq faces (list faces)))
	(when (memq 'font-lock-function-name-face faces)
	  (setq not-yet nil))))
    pos))

(defun daily-memo-get-function-name-pos-end (beg)
  (let ((faces ())
	(not-yet t)
	(eol (daily-memo-get-pos-eol beg))
	(pos (1- beg)))
    (while not-yet
      (setq pos (1+ pos))
      (if (= pos eol)
	  (progn
	    (setq pos nil)
	    (setq not-yet nil))
	(setq faces (get-text-property pos 'face))
	(unless (listp faces)
	  (setq faces (list faces)))
	(unless (memq 'font-lock-function-name-face faces)
	  (setq not-yet nil))))
    pos))

(defun daily-memo-get-function-name ()
  (save-excursion
    (when (eq major-mode 'c-mode)
      (beginning-of-defun)
      (forward-line -1)
      (beginning-of-line)
      (let (beg end str)
	(setq beg (daily-memo-get-function-name-pos-begin (point)))
	(when beg
	  (setq end (daily-memo-get-function-name-pos-end beg)))
	(if (and (and beg end)
		 (and (integerp beg) (integerp end)))
	    (buffer-substring beg end)
	  nil)))))

;; get file name
(defun daily-memo-get-buffer-file-name ()
  (let ((str (buffer-file-name)))
    (if str
	(setq str (file-name-nondirectory str))
      nil)))

;; insert region to memo buffer
(defun daily-memo-insert-region (rgn-beg rgn-end)
  (interactive "r")
  (if (narrowing-p)
      (message "narrowing now. please widen then try again.")
    (let ((file-name (daily-memo-get-buffer-file-name))
	  (func-name (daily-memo-get-function-name))
	  (line-num (get-current-line-number rgn-beg))
	  (curr-buf (buffer-name))
	  (memo-buf (daily-memo-open))
	  beg end)
      (when memo-buf
	(setq func-name (if func-name
			    (concat func-name "() ")
			  (setq func-name "")))
	(setq beg (point))
	(insert daily-memo-comment-string
		func-name
		"@" file-name
		":L" (number-to-string line-num)
		(string ?\n))
	(insert-buffer-substring curr-buf rgn-beg rgn-end)
	(newline)
	(unless (= (current-column) 0) (newline))
	(setq end (point))

	;; replace tab --> space
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (replace-string (string ?\t) " ")))))))

;; open daily memo buffer
(defun daily-memo-open ()
  (interactive)
  (setq daily-memo-prev-buffer (current-buffer))
  (let* ((dir (directory-file-name (expand-file-name daily-memo-file-directory)))
	 (date-str (format-time-string "%y%m%d" (current-time)))
	 (memo-file (concat dir "/" date-str ".txt"))
	 (memo-buf nil))
    ;; check buffers (todo)
;;     (if (buffer-exists-by-file-name-p memo-file)
;; 	(set-window-buffer (selected-window) )
    
    ;; check memo-dir
    (if (and (file-exists-p dir) (file-directory-p dir) (file-writable-p dir))
	;; check memo-file
	(progn
	  (if (file-exists-p memo-file)
	      (setq memo-buf (find-file memo-file))
	    (setq memo-buf (get-buffer-create memo-file))
	    (switch-to-buffer memo-buf))
	  (set-visited-file-name memo-file)
	  (goto-char (point-max))
	  (unless (= (current-column) 0)
	    (newline))
	  (when (interactive-p)
	    (insert daily-memo-comment-string))
	  (set-buffer-modified-p nil)
	  (define-key text-mode-map "\C-xk" 'daily-memo-kill-buffer)
	  (define-key text-mode-map "\C-m" 'newline)
	  memo-buf) 			; return memo-buf
      (message "invalid memo-directory")
      nil)))				; return nil

(defun daily-memo-kill-buffer ()
  "Kill buffer with automatic save."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (kill-buffer-current)
  (when daily-memo-prev-buffer
    (switch-to-buffer daily-memo-prev-buffer)))


(global-set-key "\C-xm" 'daily-memo-open) ;C-x m
(global-set-key "\C-xi" 'daily-memo-insert-region) ;C-x i

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ insert-my-vss-signature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar insert-my-vss-signature-string "HSHA")
;; (defun insert-my-vss-signature ()
;;   "insert string for vss signature like sig:ZZR / 2006-12-30 -->`061230ZZR\'"
;;   (interactive)
;;   (insert (concat (format-time-string "%y%m%d" (current-time))
;; 		  insert-my-vss-signature-string)))

;; (global-set-key "\C-c\C-j" 'insert-my-vss-signature)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ highlight-current-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun highlight-current-line ()
  (let (beg end ol)
    (save-excursion			;get beg, end
      (setq beg (progn (beginning-of-line) (point)))
      (setq end (progn (end-of-line) (point))))
    (setq ol (make-overlay beg end))	;hilight line
    (overlay-put ol 'face 'highlight)
    (sit-for 5)
    (delete-overlay ol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-previous-line and my-next-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(progn    ; comment-beg
(defvar next-prev-line-highlight nil)
(make-variable-buffer-local 'next-prev-line-highlight)

(defun previous-line (arg)
  "My customized previous-line"
  (interactive "p")
  (if (interactive-p)
      (progn
	(condition-case nil
	    (unless (= (narrowed-current-line) 1)
	      (line-move (- arg)))
	  (goto-char (point-min)))
	(when next-prev-line-highlight
	  (highlight-current-line)))
    (line-move (- arg)))
  nil)

(defun narrowed-current-line ()
  "Print the current buffer line number and narrowed line number of point."
  (let ((opoint (point)) start)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(widen)
	(forward-line 0)
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(1+ (count-lines start (point)))))))

(defun next-line (arg)
  "My customized next-line"
  (interactive "p")
;;   (if (interactive-p)
;;       (progn
	;(call-interactively 'next-line)
;; 	(line-move arg)
;; 	(when next-prev-line-highlight
;; 	  (highlight-current-line)))
;; 	)
  (let ((noerr (if (active-minibuffer-window) t nil)))
    (line-move arg noerr)))
;;   nil)

) ; comment-end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ delete-spaces-then-newline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-spaces-then-newline ()
  "delete all spaces and tabs prior newline."
  (interactive)
  (while (and (/= (point) (point-min))
	      (= ?\  (char-syntax (char-before (point)))))
    (delete-backward-char 1))
  (newline))

;; (global-set-key "\C-m" 'delete-spaces-then-newline) ; enter
;; (global-set-key "\M-\C-m" 'newline)		    ; alt + enter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ insert-tab-character
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-tab-character ()
  (interactive)
  (insert "\t"))

;; bind to C-tab
;; (global-set-key (quote [C-tab]) 'insert-tab-character)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ insert-key-string etc...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-key-string (key)
  (let* ((str (describe-key-briefly key))
	 (end (string-match " runs " str)))
    (unless end
      (setq end (string-match " is " str)))
    (substring str 0 end)))

(defvar insert-key-string (list "" " "))
(defun insert-key-string (key)
  (interactive "kkey:")
  (insert (car insert-key-string)
	  (get-key-string key)
	  (car (cdr insert-key-string))))

(defun get-command-string (key)
  (let* ((str (describe-key-briefly key)))
    (string-match " command " str)
    (setq beg (match-end 0))
    (if beg
	(substring str beg)
      (""))))

(defvar insert-command-string (list "" ""))
(defun insert-command-string (key)
  (interactive "kkey:")
  (insert (car insert-command-string)
	  (get-command-string key)
	  (car (cdr insert-command-string))))

(defvar insert-key-command-string (list "" "\t\t" "\n"))
(defun insert-key-command-string (key)
  (interactive "kkey:")
  (insert (car insert-key-command-string)
	  (get-key-string key)
	  (nth 1 insert-key-command-string)
	  (get-command-string key)
	  (nth 2 insert-key-command-string)))
;; key-bind
(global-set-key "\C-x\C-y" 'insert-key-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ enum-buffer-names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enum-buffer-names ()
  "$BB8:_$9$k%P%C%U%!L>$r$9$Y$FNs5s$7!"(Bscratch$B%P%C%U%!$KI=<($9$k!#(B"
  (interactive)
  (setq buff-list (buffer-list))
  (set-buffer "*scratch*")
  (end-of-buffer)
  (insert "\n")
  (insert "---- enum-buffer-names start --------------------------\n")
  (while buff-list
    (insert "\"")
    (insert (buffer-name (car buff-list)))
    (insert "\"\n")
    (setq buff-list (cdr buff-list)))
  (insert "---- enum-buffer-names end ----------------------------\n")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ visible-whole-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun visible-whole-buffer ()
  "$B%+%l%s%H%P%C%U%!>e$G%*!<%P%l%$$,(B invisible $B$JJ8;z$r$9$Y$FI=<($9$k!#(B"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let (ol-start ol-list)
	(setq ol-start (next-overlay-change (point)))
	(setq ol-list (overlays-at ol-start))
	(mapcar (function (lambda (ol)
			    (overlay-put ol 'invisible nil))) ol-list))
      (forward-char 1))))

(defun hide-all-comment-lines ()
  "$B%+%l%s%H%P%C%U%!>e$N%3%a%s%H9T$r$9$Y$F1#$9!#(B"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while (< (point) (point-max))
      (let (start end)
	(if (whole-comment-line-p)
	    (progn
	      (setq start (progn
			    (beginning-of-line)
			    (point)))
	      (setq end (progn
			  (forward-line)
			  (point)))
	      (when (< start end)
		(overlay-put (make-overlay start end) 'invisible t)))
	  (forward-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ commentize-and-next-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun text-syntax-table-not-all (start end class)
  "start - end $B4V$G!"(Bsyntax-table$B$rD4$Y$F!";XDj$5$l$?(Bclass$B0J30$N$b$N$,(B
$B=P8=$9$k0LCV$rJV$9!#$9$Y$F$NJ8;z$,(Bclass$B$J$i(Bnil$B$rJV$9!#(B"
  (let ((pt nil)
	(ret nil))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not pt))
	(setq ret (char-syntax (char-after)))
	(unless (= class ret)
	  (setq pt (point)))
	(forward-char 1)))
    pt))

(defun text-face-not-all (start end name)
  "start - end $B4V$G!";XDj$5$l$?(Bname$B$r4^$s$G$$$J$$(Bface$B$,=P8=$9$k0LCV$rJV$9!#(B
$B$9$Y$F$NJ8;z$N(Bface$B$,(Bname$B$r4^$s$G$$$l$P(Bnil$B$rJV$9!#(B"
  (let ((pt nil)
	(faces ()))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not pt))
	(setq faces (get-text-property (point) 'face))
	(unless (listp faces)
	  (setq faces (list faces)))
	(unless (or (memq name faces) (memq 'font-lock-comment-delimiter-face faces))
	  (setq pt (point)))
	(forward-char 1)))
    pt))

(defun whole-comment-line-p ()
  "$B%+%l%s%H9T$,$9$Y$F%3%a%s%H$J$i(Bt, $B$=$&$G$J$1$l$P(Bnil$B$rJV$9!#(B
$B0lIt%3%a%s%H$,4^$^$l$F$$$k>l9g$b(Bnil$B$rJV$9!#6u9T$N>l9g$b(Bnil$B$rJV$9!#(B"
  (let (start end nc)
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq start (point)))
    (if (< start end)
      (progn
	;; $BGrJ8;z%/%i%90J30$,:G=i$K=P8=$9$k0LCV$rC5$9!#(B
	(setq start (text-syntax-table-not-all start end 32)) ;; 32$B$OGrJ8;z%/%i%9(B
	(if start
	    (setq nc (text-face-not-all start end 'font-lock-comment-face))
	  (setq nc nil))
	(when (integerp nc)
	  (setq nc (text-face-not-all start end 'font-lock-comment-delimiter-face)))
	(if (integerp nc)
	    nil
	  t))
      nil)))

(defun commentize-and-next-line (&optional ARG)
  "$B%+%l%s%H9T$r%3%a%s%H2=!?%"%s%3%a%s%H2=$7!"<!$N9T$X0\F0$9$k!#(B
C-u$B$K$h$jA0CV0z?t$r;H$&$H<!$N9T$K0\F0$7$J$$!#(B
$B%f!<%6JQ?t(B commentize-and-next-line-set-mark $B$,(B nil$B0J30$N$H$-$O(B
$B%3%a%s%H2=$9$k:G=i$N9T$r%^!<%/$9$k!#!J%G%U%)%k%H!K(B"
  (interactive "P")
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point)))
    (when (< beg end)
      (if (whole-comment-line-p)
	  (uncomment-region beg end)
	(comment-region beg end))))
  (unless ARG
    (when (and commentize-and-next-line-set-mark
	       (not (eq last-command 'commentize-and-next-line)))
      (push-mark))
    (forward-line 1)
    (beginning-of-line)))

(global-set-key "\M-;" 'commentize-and-next-line)

(defvar commentize-and-next-line-set-mark t
  "*nil$B0J30$N$H$-$O!"%3%a%s%H2=$9$k:G=i$N0LCV$r%^!<%/$9$k!#(B
C-x C-x (exchange-point-and-mark) $BEy$GJXMx!#(B")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ indent-for-comment (from xyzzy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-for-comment-gnrr (&optional ARG)
  (interactive "P")
  (if ARG
      ;; set-comment-column
      (let ((col (current-column)))
        (setq comment-column col)
        (message "set comment-column to %d." col))
    (indent-for-comment)
    (when (eq evil-state 'normal)
      (evil-insert 1))))

(global-set-key [?\C-\;] 'indent-for-comment-gnrr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ comment-deco
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun comment-deco ()
;;   (interactive)
;;   (let ((beg nil)
;; 	(end nil)
;; 	(col nil)
;; 	(deco-char (aref comment-start 0)))
;;     (save-excursion
;;       (beginning-of-line)
;;       (setq beg (point))
;;       (end-of-line)
;;       (setq end (point)))
;;     (if (whole-comment-line-p)
;; ;;	already comment line
;; 	(progn
;; 	  (uncomment-region beg end)
;; 	  (end-of-line)
;; 	  (while (or (= (char-before) 32) (= (char-before) deco-char))
;; 	    (backward-delete-char 1)))
;; ;;      comment-start
;;       (comment-region beg end)
;; ;;      comment-end
;;       (end-of-line)
;;       (while (= (char-before) 32) ;; space?
;; 	(backward-delete-char 1))
;;       (insert 32) ;; space x1
;;       (setq col (- comment-deco-columns (current-column)))
;;       (while (> col 0)
;; 	(insert deco-char)
;; 	(setq col (- col 1)))))
;;   (next-line 1)
;;   (beginning-of-line))
;;
;; (global-set-key "\M-:" 'comment-deco)
;;
;; (defvar comment-deco-columns 70
;;   "*Comment line columns")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-copy-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-copy-word-thing-n 0)
(make-variable-buffer-local 'my-copy-word-thing-n)
(defvar my-copy-word-prev-str "")
(make-variable-buffer-local 'my-copy-word-prev-str)
(defvar my-copy-word-ini-str "")
(make-variable-buffer-local 'my-copy-word-ini-str)

(defun my-copy-word ()
  (interactive)
  (let ((sel '(word symbol filename))
	(n my-copy-word-thing-n)
	(str nil)
	beg end ol)
    (if (eq last-command 'my-copy-word)
	(if (< n (1- (length sel)))
	    (setq n (1+ n))
	  (setq n 0))
      (setq n 0))
    (while (or (null (prog1
			 (setq str (thing-at-point (nth n sel)))
		       (set-text-properties 0 (length str) nil str)))
	       (and (string= str my-copy-word-prev-str)
		    (not (string= str my-copy-word-ini-str))))
      (if (< n (1- (length sel)))
	  (setq n (1+ n))
	(setq n 0)))
    (kill-new str)			;copy to kill-ring
    (setq my-copy-word-thing-n n)
    (setq my-copy-word-prev-str str)
    (setq my-copy-word-ini-str (when (= n 0) str))
    (message "copied: %s" str)
    (save-excursion			;get beg, end
      (let ((len (length str)))
	(while (null (progn
		       (setq beg (point))
		       (setq end (+ beg len))
		       (string= str
				(buffer-substring-no-properties beg end))))
	  (backward-char))))
    (setq ol (make-overlay beg end))	;hilight word
    (overlay-put ol 'face 'highlight)
    (sit-for 5)
    (delete-overlay ol)))

; key-bind
(global-set-key [?\C-=] 'my-copy-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-save-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create parent directories when saving a new file
(defvar my-save-buffer-default-path nil
  "default directory where to save")
(make-variable-buffer-local 'my-save-buffer-default-path)

(defun create-directory (directory)
  (if (y-or-n-p (format "Parent directory %s not exists, Create? " directory))
      (let* ((dl "/")
	     (dire "")
	     (list (split-string directory dl)))
	(while list
	  (setq dire (concat dire (car list) dl))
	  (unless (file-exists-p dire)
	    (make-directory (directory-file-name dire)))
	  (setq list (cdr list)))
	t)
    nil))

(defvar my-save-buffer-interactive-arg-active-p nil)
(defun my-save-buffer-interactive-arg (&optional initial)
  (let* ((my-save-buffer-interactive-arg-active-p t)
	 (insert-default-directory (null initial)))
    (if (buffer-modified-p)
	(if buffer-file-name
	    ; buffer has a file-name
	    (let* ((name (expand-file-name buffer-file-name))
		   (directory (file-name-directory name)))
	      (if (file-exists-p directory)
		  (progn (save-buffer)
			 'done)
		(if (create-directory directory)
		    (progn (save-buffer)
			   'done)
		  (error "Abort"))))
	  ; buffer does not have a file-name
	  (let* ((rd (read-file-name "File to save in: " nil nil nil initial))
		 (name (expand-file-name rd))
		 (directory (file-name-directory name)))
	    (if (file-exists-p name)
		(list 'already-exists rd)
	      (if (file-exists-p directory)
		  (progn (write-file name)
			 'done)
		(if (create-directory directory)
		    (progn (write-file name)
			   'done)
		  (list 'cant-create-parent-dir rd))))))
      'buffer-no-mod)))

(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (when my-save-buffer-interactive-arg-active-p
	       (end-of-line))))

(defun my-save-buffer ()
  (interactive
   (let ((nyet t)
	 (wait 1)
	 r)
     (while nyet
       (setq r (my-save-buffer-interactive-arg my-save-buffer-default-path))
       (if (listp r)
	   (let ((err (car r))
		 (inp (cadr r)))
	     (cond ((eq err 'already-exists)
		    (message "\(File already exists\)") (sit-for wait))
		   ((eq err 'cant-create-parent-dir)
		    (message "\(Can not create parent directory\)") (sit-for wait))
		   ((eq err 'buffer-no-mod)
		     (setq nyet nil))))
	 (when (symbolp r)
	   (when (eq r 'buffer-no-mod)
	     (message "\(No changes need to be saved\)"))
	   (setq nyet nil)))))
   nil))

;; key-bind
(global-set-key "\C-x\C-s" 'my-save-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-write-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create parent directories when writing a new file
(defvar my-write-file-interactive-arg-active-p nil)

;; (defun my-write-file-write-proc (path)
;;   (if (file-exists-p path)
;;       ;$BF1$8%U%!%$%kL>$,4{$KB8:_$7$F$$$k>l9g(B
;;       (progn
;;         (message "already exists")
;;         (sit-for 5)
;;         (my-write-file-interactive-arg path))
;;     ;$BF1$8%U%!%$%kL>$,B8:_$7$J$$>l9g(B
;;     (write-file path)))

(defun my-write-file-interactive-arg (&optional initial)
  (let* ((my-write-file-interactive-arg-active-p t)
	 (insert-default-directory nil)
	 (path initial))
    (when (and (not initial) (buffer-file-name))
      (setq path (buffer-file-name)))
    (let* ((name (expand-file-name (read-file-name "File to save in: "
						   nil nil nil path)))
	   (directory (file-name-directory name)))
      (if (file-exists-p directory)
	  ;$B%G%#%l%/%H%j$,$9$G$KB8:_$7$F$$$k>l9g(B
	  (write-file name t)
	;$B%G%#%l%/%H%j$,B8:_$7$J$$>l9g(B
	(if (create-directory directory)
	    ;$B%G%#%l%/%H%j$r:n$l$?>l9g(B
	    (my-write-file-interactive-arg name)
	  ;$B%G%#%l%/%H%j$r:n$l$J$+$C$?>l9g(B
	  (my-save-buffer-interactive-arg name))))))

(defun my-write-file-minibuffer-setup-hook ()
  (when my-write-file-interactive-arg-active-p
    (end-of-line)))

(add-hook 'minibuffer-setup-hook
	  'my-write-file-minibuffer-setup-hook)

(defun my-write-file ()
  (interactive (my-write-file-interactive-arg)))

(global-set-key "\C-x\C-w" 'my-write-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-find-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thank somebody at 2ch.
(defvar my-find-file-interactive-arg-active-p nil)
(defun my-find-file-interactive-arg (str &optional initial)
  (let* ((my-find-file-interactive-arg-active-p t)
	 (insert-default-directory (null initial))
	 (name (read-file-name str nil nil nil initial))
	 (wild-p current-prefix-arg)
	 (file (if wild-p name (expand-file-name name))))
    (cond ((if wild-p (file-expand-wildcards name)
			 (file-exists-p file))
		   (list file wild-p))
		  ((y-or-n-p (format "%s not exists, New file? " (file-name-nondirectory file)))
		   (list file wild-p))
		  (t (my-find-file-interactive-arg "Find file: " name)))))

(defun my-find-file-minibuffer-setup-hook ()
  (when my-find-file-interactive-arg-active-p
    (end-of-line)))

(add-hook 'minibuffer-setup-hook
	  'my-find-file-minibuffer-setup-hook)

(defun my-find-file (filename &optional wildcards)
  (interactive (my-find-file-interactive-arg "Find file: "))
  (find-file filename wildcards))

(defun my-find-alternate-file (filename &optional wildcards)
  (interactive (my-find-file-interactive-arg "Find alternate file: "))
  (find-alternate-file filename wildcards))

(defun my-find-file-pre-process (arg)
  "switch `my-find-file' or `my-find-alternate-file' by arg."
  (interactive "P")
  (call-interactively (if arg
			  'my-find-alternate-file
			'my-find-file)))

(global-set-key "\C-x\C-f" 'my-find-file-pre-process)
(global-unset-key "\C-x\C-v")		; find-alternate-file


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-insert-filename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-insert-filename-start "Filename:"
  "*The filename string is inserted following this string.")
(defvar my-insert-filename-lines 10
  "*Its applied upto this line number from head of file.")

(defun my-insert-filename ()
  "Insert filename like as time-stamp."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (progn
				      (goto-char (point-min))
				      (end-of-line)
				      (let ((lines my-insert-filename-lines))
					(while (and (not (eobp)) (> lines 1))
					  (next-line 1)
					  (end-of-line)
					  (setq lines (1- lines))))
				      (point)))
      (goto-char (point-min))
      (let ((cs case-fold-search))
	;;case insesitive
	(setq case-fold-search nil)
	(when (search-forward my-insert-filename-start nil t)
	  (while (not (eolp))
	    (delete-char 1))
	  (insert " ")
	  (insert (file-name-nondirectory buffer-file-name)))
	(setq case-fold-search cs))))
  nil)

(add-hook 'write-file-hooks 'my-insert-filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ trim-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; http://kwatch.tripod.co.jp/mule/mule.13.html $B$,85(B
(defvar trim-buffer-enable t
  "*Non-nil if this buffer is applied trim-buffer when save-buffer.
;; ;; Each buffer has its own value of this variable.")
(make-variable-buffer-local 'trim-buffer-enable)

(defvar trim-buffer-except-exts-all '("^[^.]+$")
  "*List of extensions should be excepted from all functional of trim-buffer.")
(defvar trim-buffer-except-exts-del-trail-spc nil
  "*List of extensions should be exceptped delete trailing white spaces.")
(defvar trim-buffer-except-exts-del-empty-end-line nil
  "*List of extensions should be exepted delete empty line of the end of buffer.")
(defvar trim-buffer-tabify-list nil
  "*Define extensions want to be tabified.")
(defvar trim-buffer-untabify-list nil
  "*Define extensions want to be untabified.")

(defun toggle-trim-buffer ()
  (interactive)
  (if trim-buffer-enable
      (progn
	(setq trim-buffer-enable nil)
	(message "Trim buffer: off"))
    (setq trim-buffer-enable t)
    (message "Trim buffer: on")))

(defun trim-buffer-correspond-ext-p (filename list)
  (when filename
    (let ((match nil))
      (while (and list (null match))
	(when (string-match (car list) filename)
	    (setq match t))
	(setq list (cdr list)))
      match)))

(defun trim-buffer-del-trail-spc ()
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match "" nil nil)))

(defun trim-buffer-del-empty-end-line ()
  (goto-char (point-max))
  (delete-blank-lines))

(defun trim-buffer-tabify ()
  (mark-whole-buffer)
  (tabify (region-beginning) (region-end)))

(defun trim-buffer-untabify ()
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end)))

(defun trim-buffer ()
  "Delete excess white space, and also doing tabify or untabify."
  (interactive)
    (if (interactive-p)
	;; interactively
	(save-excursion
	  (trim-buffer-del-trail-spc)
	  (trim-buffer-del-empty-end-line)
	  (trim-buffer-tabify))

      ;; not interactively (called from write-file-hooks)
      (let ((filename (file-name-nondirectory buffer-file-name)))
	(when (and trim-buffer-enable
		   (not (trim-buffer-correspond-ext-p
			 filename
			 trim-buffer-except-exts-all)))
	  (save-excursion
	    ;; $B9TKv$N6uGr$r:o=|$9$k(B
	    (unless (trim-buffer-correspond-ext-p
		     filename
		     trim-buffer-except-exts-del-trail-spc)
	      (trim-buffer-del-trail-spc))

	    ;; $B%U%!%$%k$N=*$o$j$K$"$k6uGr9T$r:o=|$9$k(B
	    (unless (trim-buffer-correspond-ext-p
		     filename
		     trim-buffer-except-exts-del-empty-end-line)
	      (trim-buffer-del-empty-end-line))

	    ;; untabify or tabify
	    (cond
	     ;; tabify
	     ((trim-buffer-correspond-ext-p filename trim-buffer-tabify-list)
	      (trim-buffer-tabify))
	     ;; untabify
	     ((trim-buffer-correspond-ext-p filename trim-buffer-untabify-list)
	      (trim-buffer-untabify))))))
      )
    nil)

;; (add-hook 'write-file-hooks 'trim-buffer)
;; (global-set-key "\C-t" 'trim-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-just-one-space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar my-just-one-space-state nil)
;; (defun my-just-one-space ()
;;   (interactive)
;;   (if (and (eq last-command 'my-just-one-space) (null my-just-one-space-state))
;;       (progn
;; 	(backward-delete-char 1)
;; 	(setq my-just-one-space-state t))
;;     (progn
;;       (just-one-space)
;;       (setq my-just-one-space-state nil))))

;; (global-set-key "\M- " 'my-just-one-space)
;; (global-set-key "\C- " 'my-just-one-space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-mark-whole-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Similarly C-a under WINDOWS
;;(defvar my-mark-whole-buffer-region ())

;; (defun my-mark-whole-buffer ()
;;   "Usefull mark-whole-buffer."
;;   (interactive)
;;   (setq my-mark-whole-buffer-region (cons (point-min) (point-max)))
;;   (message "Selected all"))
;;
;; (global-set-key "\C-x\C-a" 'my-mark-whole-buffer)
;; (global-set-key "\M-a"     'my-mark-whole-buffer)

;; (defun my-kill-region ()
;;   (interactive)
;;   (if (eq last-command 'my-mark-whole-buffer)
;;       (let ((region my-mark-whole-buffer-region))
;;         (kill-region (car region) (cdr region)))
;;     (call-interactively 'kill-region)))
;;
;; (global-set-key "\C-w" 'my-kill-region)

;; (defun my-kill-ring-save ()
;;   (interactive)
;;   (if (eq last-command 'my-mark-whole-buffer)
;;       (let ((region my-mark-whole-buffer-region))
;;         (kill-ring-save (car region) (cdr region)))
;;     (call-interactively 'kill-ring-save)))
;;
;; (global-set-key "\M-w" 'my-kill-ring-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ toggle-narrowing-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar toggle-narrowing-region-window-start nil)
(defvar toggle-narrowing-region-previous-rend nil)
(defun toggle-narrowing-region (beg end)
  "Toggle narrowing/widening region."
  (interactive "r")
  (if (narrowing-p)
      ; now nallowed
      (progn
	(widen)
;; 	(when (integerp toggle-narrowing-region-window-start)
;; 	  (set-window-start nil toggle-narrowing-region-window-start))
	(message "<< Widened >>"))
    ; now widened
    (let ((rend end))
      (when (and (= beg end)
		 (integerp toggle-narrowing-region-previous-rend))
	(setq rend toggle-narrowing-region-previous-rend))
      (if (= beg rend)
	  (message "No region.")
	(setq toggle-narrowing-region-window-start (window-start))
	(narrow-to-region beg rend)
	(goto-char (point-min))
	(setq toggle-narrowing-region-previous-rend rend)
	(message ">> Narrowed <<")))))

(global-set-key "\C-xnn" 'toggle-narrowing-region)
(global-unset-key "\C-xnw")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ insert-paren-*    [], {}, <>, "", ''
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar insert-paren-kaku-state nil)
;; (defun insert-paren-kaku ()
;;   "Insert paired [] or {} working like insert-parenthesis."
;;   (interactive)
;;   (if (eq last-command 'insert-paren-kaku)
;;       (progn
;; 	(forward-char -1)
;; 	(delete-char 2)
;; 	(if (null insert-paren-kaku-state)
;; 	    (progn
;; 	      (insert ?\{)
;; 	      (save-excursion
;; 		(insert ?\}))
;; 	      (setq insert-paren-kaku-state t))
;; 	  (progn
;; 	    (insert ?\[)
;; 	    (save-excursion
;; 	      (insert ?\]))
;; 	    (setq insert-paren-kaku-state nil))))
;;     (progn
;;       (insert ?\[)
;;       (save-excursion
;; 	(insert ?\]))
;;       (setq insert-paren-kaku-state nil))))

(defun insert-paren-kaku ()
  "Insert paired [] or {} working like insert-parenthesis."
  (interactive)
  (if (eq last-command 'insert-paren-kaku)
      (progn
	(forward-char -1)
	(delete-char 2)
	(if (null insert-paren-kaku-state)
	    (progn
	      (insert ?\[)
	      (save-excursion
		(insert ?\]))
	      (setq insert-paren-kaku-state t))
	  (progn
	    (insert ?\{)
	    (save-excursion
	      (insert ?\}))
	    (setq insert-paren-kaku-state nil))))
    (progn
      (insert ?\{)
      (save-excursion
	(insert ?\}))
      (setq insert-paren-kaku-state nil))))

(global-set-key "\M-[" 'insert-paren-kaku)

(defvar insert-paren-quote-state nil)
(defun insert-paren-quote ()
  "Insert paired single-quote or double-quote working like insert-parenthesis."
  (interactive)
  (if (eq last-command 'insert-paren-quote)
      (progn
	(forward-char -1)
	(delete-char 2)
	(if (null insert-paren-quote-state)
	    (progn
	      (insert ?\")
	      (save-excursion
		(insert ?\"))
	      (setq insert-paren-quote-state t))
	  (progn
	    (insert ?\')
	    (save-excursion
	      (insert ?\'))
	    (setq insert-paren-quote-state nil))))
    (progn
      (insert ?\')
      (save-excursion
	(insert ?\'))
      (setq insert-paren-quote-state nil))))
(global-set-key "\M-'" 'insert-paren-quote)

(defun insert-paren-gtlt()
  "Insert paired <> working like insert-parenthesis."
  (interactive)
  (insert ?\<)
  (save-excursion
    (insert ?\>)))
(global-set-key "\M-<" 'insert-paren-gtlt)
(global-unset-key "\M->")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ beginning-of-buffer-without-marking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beginning-of-buffer-without-marking ()
  "more simple beginning-of-buffer without marking."
  (interactive)
  (goto-char (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ end-of-buffer-without-marking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun end-of-buffer-without-marking ()
  "more simple end-of-buffer without marking."
  (interactive)
  (goto-char (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-forward-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my-forward-word ()
;;   "For people who dislike working of default forward-word."
;;   (interactive)
;;   (let ((nword 1))
;;     (when (string= (string (char-syntax (char-after))) "w")
;;	 (setq nword 2))
;;   (forward-word nword)
;;   (forward-word -1)))

;; (global-set-key "\M-f" 'my-forward-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-kill-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-kill-word ()
  "For people who dislike working of default kill-word."
  (interactive)
  (let ((start-class (char-syntax (char-after))))
    (while (= start-class (char-syntax (char-after)))
      (delete-char 1))))

(global-set-key "\M-d" 'my-kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-backward-kill-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my-backward-kill-word ()
;;   "For people who dislike working of default backward-kill-word."
;;   (interactive)
;;   (let ((start-class (char-syntax (char-before))))
;;     (while (= start-class (char-syntax (char-before)))
;;       (backward-delete-char 1)
;;       (when (= 10 (char-before)) ;; 10 = carriage return
;; 	(setq start-class 0)))))

;; (global-set-key "\M-h" 'my-backward-kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-backward-kill-word-minibuffer
;;  and some fix for me
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-backward-kill-word-minibuffer-sub (delimiter)
  "for path delimiter"
  (let (buf)
    (when (> (point) (minibuffer-prompt-end))
      (setq buf (concat (string (char-before)) buf))
      (backward-delete-char 1))
    (while (and (not (= delimiter (char-before)))
		(> (point) (minibuffer-prompt-end)))
      (setq buf (concat (string (char-before)) buf))
      (backward-delete-char 1))
    (setq dir-list (cons buf dir-list))))

(defun my-backward-kill-word-minibuffer ()
  "replace for path delimiter, word delimiter, white space "
  (interactive)
  (let ((ch (cond
	     ;; elisp functions --> "-"
	     ((and minibuffer-completion-table
		   (sequencep minibuffer-completion-table))
	      ?-)
	     ;; path string --> "/"
	     ((eq minibuffer-completion-table 'read-file-name-internal)
	      ?/)
	     ;; others --> " "
	     (t ?\ ))))
    (my-backward-kill-word-minibuffer-sub ch)))

(defun my-forward-word-minibuffer ()
  "undo path string"
  (interactive)
  (unless (null dir-list)
    (insert (car dir-list))
    (setq dir-list (cdr dir-list))))

(defun my-backward-delete-char-minibuffer ()
  "replace backward-delete-char-minibuffer"
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (backward-delete-char 1)))

(defun my-backward-char-minibuffer ()
  "replace backward-char-minibuffer"
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (backward-char 1)))

(defun my-forward-char-minibuffer ()
  "replace forward-char-minibuffer"
  (interactive)
  (when (< (point) (point-max))
    (forward-char 1)))


;; (add-hook 'minibuffer-setup-hook
;; 	    (function
;; 	      (lambda ()
;; 		(setq dir-list '())
;; 		(local-set-key "\M-h" 'my-backward-kill-word-minibuffer)
;; 		(local-set-key "\M-f" 'my-forward-word-minibuffer)
;; 		(local-set-key "\C-h" 'my-backward-delete-char-minibuffer)
;; 		(local-set-key "\C-b" 'my-backward-char-minibuffer)
;; 		(local-set-key "\C-f" 'my-forward-char-minibuffer)
;; 		)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ select previous window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun other-window-backwards (&optional n)
  "Select Nth previous window."
  (interactive "p")
  (other-window (if n (- n) -1)))

(global-set-key "\C-xp" 'other-window-backwards)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ switch to previous buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun return-prev-buffer (b-list)
  "Return previous buffer excluding *Help* or *Messages* or *etc*..."
  (while b-list
    (let* ((buf (car b-list))
	  (buf-name (buffer-name buf)))
      (if (string-match "^[\\*]" buf-name)
	  (bury-buffer buf)))
    (setq b-list (cdr b-list)))
  (let* ((cand-buf (other-buffer nil))
	 (cand-buf-name (buffer-name cand-buf)))
    (if (or (string-match "^[\\*]" cand-buf-name)
	    (string-match "Electric Buffer List" cand-buf-name))
	(get-buffer-create "*scratch*")
      cand-buf)))

(defun switch-to-previous-buffer (&optional arg)
  "No need to specify buffer name in order to switch to previous buffer."
  (interactive "P")
  (if arg
      (switch-to-buffer nil) ;; hmmm... i wish i could make default working
    (let ((buf (return-prev-buffer (buffer-list))))
      (switch-to-buffer buf))))

;;(global-set-key "\C-xb" 'switch-to-previous-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'redo)
(defun my-undo-redo (&optional ARG)
  "This function invocates either undo or redo according to ARG.
When ARG is nil, undo is called.
Otherwise,  ARG is t, redo is called."
  (interactive "*P")
  (if ARG
      (redo 1)
    (undo 1)))

(global-set-key "\C-z" 'my-undo-redo)
(global-set-key "\M-z" 'my-undo-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ find the next tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun find-tag-next ()
;;   "Search for another tag that matches the last tagname or regexp used."
;;   (interactive)
;;   (find-tag tags-file-name t))

;; (global-set-key "\M-," 'find-tag-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ multiply current line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun duplicate-line (&optional ARG)
  "Multiply current line."
  (interactive"*p")
  (let ((cnt 0)
	(pt (point)))
    (save-excursion
	(end-of-line)
	(setq str
	      (buffer-substring-no-properties (point) (progn
							(beginning-of-line)
							(point))))
	(if (null ARG)
	    (setq ARG 1))
	(while (< cnt ARG)
	  (insert str)
	  (newline)
	  (setq cnt (1+ cnt))))
    (goto-char pt)
    (next-line 1)))

(global-set-key [?\M-=] 'duplicate-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ set-mark w/ fringe-indicator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fringe-indicator-ol nil)
(defun fringe-indicator (pt bitmap)
  (let ((s (make-string 1 ?x)))
	(when fringe-indicator-ol (delete-overlay fringe-indicator-ol))
	(setq fringe-indicator-ol (make-overlay pt (1+ pt)))
	(put-text-property 0 1 'display (list 'left-fringe bitmap) s)
	(overlay-put fringe-indicator-ol 'before-string s)))

(defadvice set-mark-command (after fringe-indicator-adv activate)
  "indicate mark-position at fringe."
  (fringe-indicator (point) 'right-triangle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		    SOMEBODY WROTE. THANKS A LOT.		   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-calc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://d.hatena.ne.jp/uhiaha888/20110117/1295273922
(defun my-calc (beg end)
  (interactive (list (point) (mark t)))
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (calculator)
    (calc-grab-region beg end nil)))
(global-set-key "\C-c\C-c" 'my-calc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ color-yank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:highlight-string
(when (or window-system (eq emacs-major-version '21))
  (defadvice yank (after ys:highlight-string activate)
    (let ((ol (make-overlay (mark t) (point))))
      (overlay-put ol 'face 'highlight)
      (sit-for 60)
      (delete-overlay ol)))
  (defadvice yank-pop (after ys:highlight-string activate)
    (when (eq last-command 'yank)
      (let ((ol (make-overlay (mark t) (point))))
        (overlay-put ol 'face 'highlight)
        (sit-for 60)
        (delete-overlay ol)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ create tag-file automatically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=tagsfile%20maker
;; (defadvice find-tag (before c-tag-file activate)
;;   "Automatically create tags file."
;;   (let ((tag-file (concat default-directory "TAGS")))
;;     (unless (file-exists-p tag-file)
;;       (shell-command "etags *.[ch] *.el .*.el -o TAGS 2>/dev/null"))
;;     (visit-tags-table tag-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ face-file edited-today(for dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.bookshelf.jp/soft/meadow_25.html#SEC261
(defface face-file-edited-today '((t (:foreground "hot pink"))) nil)
(defvar face-file-edited-today 'face-file-edited-today)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (concat (format-time-string "%b %e" (current-time)) " [0-9]....") arg t))

(add-hook 'dired-mode-hook
	  '(lambda ()
	     (font-lock-add-keywords
	      major-mode
	      (list
	       '(my-dired-today-search . face-file-edited-today)
	       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ browse-url-of-find-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=find-file%20which
(load "which")
(defun browse-url-of-find-file (&optional arg)
  (interactive "P")
  (let ((file))
    (setq file (thing-at-point 'filename))
    (cond
     ;; $B%U%!%$%k$,B8:_$9$k$+(B?
     ((file-exists-p file)
      ())

     ;; html $B%U%!%$%k$J$i(B # $B$r:o=|$9$k(B
     ((and
       (string-match "htm[l]*#" file)
       (file-exists-p (concat (substring file 0 (string-match "#" file)) "")))
      (setq file (concat (substring file 0 (string-match "#" file)) "")))

     ;; which $B$GC5$7$F$_$k(B
     ((car (make-which-list file))
      (setq file (car (make-which-list file))))
     )

    ;; elisp $B$J$i(B el $B$r3+$$$F$[$7$$(B
    (if (and
	 (string-match "elc" file)
	 (file-exists-p (concat (substring file 0 (string-match "elc" file)) "el")))
	(setq file (concat (substring file 0 (string-match "elc" file)) "el")))

    (if (file-exists-p file)
	(find-file file)
      (if (string-match "\\(ftp\\|http\\|https\\)://[^\\.]+\\.[^\\.]+" (thing-at-point 'url))
	  (progn
	    (setq file (thing-at-point 'url))
	    (if (string-match ",.+" file)
		(setq file (concat (substring file 0 (string-match ",.+" file)) "")))
	    ;;(if arg
	    ;;	  (w3m file)
	    (browse-url file)
	    ;;	)
	    )
	(message "no file and url")))
    ))
(global-set-key "\M-j" 'browse-url-of-find-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-time-lag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#speed
(defun my-time-lag ()
  (let* ((now (current-time))
	 (min (- (car now) (car my-time-zero)))
	 (sec (- (car (cdr now)) (car (cdr my-time-zero))))
	 (msec (/ (- (car (cdr (cdr now)))
		     (car (cdr (cdr my-time-zero))))
		  1000))
	 (lag (+ (* 60000 min) (* 1000 sec) msec)))
;;    (message "'.emacs' loading time: %d msec." lag)
    (insert (format "'.emacs' loading time: %d msec." lag))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ dired-do-lha-v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;http://www.bookshelf.jp/soft/meadow_21.html#SEC217
;; ;; dired $B$G(Bt$B$H$9$k$@$1$G!$(B tar.gz $B$NFbMF$rI=<($G$-$^$9!%(B

;; (defun dired-do-tar-zvtf (arg)
;;   (interactive "P")
;;   (let ((files (dired-get-marked-files t current-prefix-arg)))
;;     (if arg
;;	(dired-do-shell-command "tar zvxf * &" nil files)
;;	 (dired-do-shell-command "tar zvtf * &" nil files))))

;; (defun dired-do-lha-v (arg)
;;   "Only one file line can be processed. If ARG, execute lha x"
;;   (interactive "P")
;;   (let ((files (mapcar (function
;;			(lambda (s) (expand-file-name s)))
;;			(dired-get-marked-files t current-prefix-arg))))
;;     (if arg
;;	(dired-do-shell-command "lha x * &" nil files)
;;	 (dired-do-shell-command "lha v * &" nil files))))

;; (defun dired-do-mandoc (arg)
;;   "man source is formatted with col -xbf. If ARG, executes without col -xbf."
;;   (interactive "P")
;;   (let ((files (dired-get-marked-files t current-prefix-arg)))
;;     (if arg
;;	(dired-do-shell-command "groff -Tnippon -mandoc * &" nil files)
;;	 (dired-do-shell-command "groff -Tnippon -mandoc * | col -xbf &" nil files))))

;; (defun dired-do-archived-file (arg)
;;   (interactive "P")
;;   (let ((file (car (dired-get-marked-files t current-prefix-arg))))
;;     (cond ((string-match "\\.tar.gz$" file)
;;	   (if arg
;;	       (dired-do-shell-command "tar zvxf * &" nil file)
;;	     (dired-do-shell-command "tar zvtf * &" nil file)))
;;	  ((string-match "\\.lzh$" file)
;;	   (if arg
;;	       (dired-do-shell-command "lha x * &" nil file)
;;	     (dired-do-shell-command "lha v * &" nil file))))))


;; (add-hook 'dired-mode-hook
;;	  '(lambda ()
;; ;;	     (define-key dired-mode-map "t" 'dired-do-tar-zvtf)
;;	     (define-key dired-mode-map "t" 'dired-do-lha-v)
;; ;;	     (define-key dired-mode-map "\eT" 'dired-do-lha-v)
;;	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ dired-toggle-mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://www.bookshelf.jp/soft/meadow_21.html#SEC208
;; $B%9%Z!<%9$G%^!<%/$9$k(B ($B%H%0%kF0:n(B)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."
  ;; S.Namba Sat Aug 10 12:20:36 1996
  (interactive "P")
  (let ((dired-marker-char
	 (if (save-excursion (beginning-of-line)
			     (looking-at " "))
	     dired-marker-char ?\040)))
    (dired-mark arg)
;;     (dired-previous-line 1)
))

(add-hook 'dired-mode-hook
	  '(lambda ()
	     (define-key dired-mode-map " " 'dired-toggle-mark)
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-keyboard-quit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#yankundo
;; keyboard-quit $B$GO"B3<B9T$9$k<oN`$N%3%^%s%I$N7k2L$r85$KLa$9(B

;; ;; dabbrev-expand $BMQ(B marker
;; (defvar my-dabbrev-marker nil)
;; (defadvice dabbrev-expand-by-category (before my-mark activate)
;;   (unless (eq last-command 'dabbrev-expand)
;;     (setq my-dabbrev-marker (point))))

;; ;; kill-ring-yank-pointer $B$N(B backup $BMQ$NJQ?t(B
;; (defvar my-kill-ring-yank-pointer-backup nil)
;; ;; yank $B$9$k$H$-$K$O(B kill-ring-pointer $B$N0LCV$r3P$($F$*$/(B
;; (defadvice yank (before my-kill-ring-yank-pointer-backup activate)
;;   (setq my-kill-ring-yank-pointer-backup kill-ring-yank-pointer))

;; (defun my-keyboard-quit ()
;;     "Wrapped keyboard-quit for yank(-pop)/dabbrev-expand/undo.
;; This command executes keyboard-quit, it deletes the inserted text when
;; the last-command is yank(-pop) or dabbrev-expand, recovers
;; kill-ring-yank-pointer when yank(-pop), and repeats redo as possible."
;;   (interactive)
;;   (cond ((eq last-command 'yank)
;;	 (let ((inhibit-read-only t))
;;	   (delete-region (point) (mark t)))
;;	 ;; yank/yank-pop $B$7$?%F%-%9%H$r>C$9(B
;;	 (setq kill-ring-yank-pointer my-kill-ring-yank-pointer-backup)
;;	 ;; kill-ring-yank-pointer $B$N0LCV$r(B yank $BA0$KLa$9(B
;;	 )
;;	((eq last-command 'dabbrev-expand)
;;	 ;; dabbrev-expand $B$7$?%F%-%9%H$r>C$9(B
;;	   (delete-region (point) my-dabbrev-marker)))
;;	((and (featurep 'redo) (eq last-command 'undo))
;;	 ;; undo $B$7$F$$$k:GCf$J$i(B redo $B$G$-$k$@$1$9$k(B
;;	   (while 1 (redo 1)))
;;	((or (eq last-command 'my-bury-buffer)
;;	     (eq last-command 'my-grub-buffer))
;;	 ;; $B%P%C%U%!$r=[4DCf$J$i:G=i$N%P%C%U%!$KLa$k(B
;;	 (switch-to-buffer (car my-visible-blst))))
;;   (keyboard-quit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ beginning-of-minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#minibuf
;;$B%_%K%P%C%U%!$K$F(B C-a $B$G%W%m%s%W%H$N8e$K0\F0$9$k$h$&$K$9$k(B
(defun beginning-of-minibuffer ()
  (interactive)
  (goto-char (minibuffer-prompt-end)))

(when (= emacs-major-version 21)
  (add-hook 'minibuffer-setup-hook
	    (function
	      (lambda ()
		(local-set-key "\C-a" 'beginning-of-minibuffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-make-scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#scratch
;;
;; *scratch* $B%P%C%U%!$G(B kill-buffer $B$7$?>l9g$O%P%C%U%!$NFbMF$r%/%j%"$7!"(B
;; scratch$B%P%C%U%!$NFbMF$rL>A0$r$D$1$FJ]B8$7$?$H$-$K$O?7$7$/(B scratch$B%P%C(B
;; $B%U%!$r:n$j$^$9!#(B
;; $B$J$*!"4V0c$C$F(B kill-buffer $B$7$FFbMF$,$+$i$C$]$K$J$C$F$7$^$C$?$H$-$O!"(B
;; erase-buffer $B$7$F$$$k$@$1$J$N$G!"(Bundo $B$9$l$P85$KLa$j$^$9!#(B

(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" $B$r:n@.$7$F(B buffer-list $B$KJ|$j9~$`(B
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
		   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
	  ((= arg 1) (message "another *scratch* is created")))))

(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

(add-hook 'kill-buffer-query-functions
	  ;; *scratch* $B%P%C%U%!$G(B kill-buffer $B$7$?$iFbMF$r>C5n$9$k$@$1$K$9$k(B
	  (function (lambda ()
		      (if (string= "*scratch*" (buffer-name))
			  (progn (my-make-scratch 0) nil)
			t))))

(add-hook 'after-save-hook
	  ;; *scratch* $B%P%C%U%!$NFbMF$rJ]B8$7$?$i(B *scratch* $B%P%C%U%!$r?7$7$/:n$k(B
	  (function (lambda ()
		      (unless (member "*scratch*" (my-buffer-name-list))
			(my-make-scratch 1)
;; 			(switch-to-buffer "*scratch*")
;; 			(tabbed-menu-shift-tab 100)
))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-create-scratch (scratch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my-create-scratch ()
;;   "*scratch* $B$,$J$1$l$P:n$k!#$9$G$K$"$l$P!"(B
;; *my-scratch* $B$H$$$&$b$&0l$D$N(B scratch $B$r:n$k!#(B
;; $B=i$a$N%a%C%;!<%8$H$+$bI|85!#(B"
;;   (interactive)
;;   (let ((scratch-buffer "*scratch*"))
;;     (if (not (get-buffer scratch-buffer))
;;	(switch-to-buffer scratch-buffer))
;; ;;	 (setq scratch-buffer "*my-scratch*"))
;;     (switch-to-buffer (get-buffer-create scratch-buffer))
;;     (funcall initial-major-mode)
;;     (if (not (buffer-modified-p))
;;	(progn
;;	  (erase-buffer)
;;	  (insert initial-scratch-message)
;;	  (set-buffer-modified-p nil)))))

;; (defalias 'scratch 'my-create-scratch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ dired extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; dired $B$G%G%#%l%/%H%j0\F0$7$F$b$=$NEY$K%P%C%U%!$r:n$i$J$$!#(B
;; (defun dired-my-advertised-find-file ()
;;   (interactive)
;;   (let ((kill-target (current-buffer))
;;	(check-file (dired-get-filename)))
;;     (funcall 'dired-advertised-find-file)
;;     (if (file-directory-p check-file)
;;	(kill-buffer kill-target))))

;; (defun dired-my-up-directory (&optional other-window)
;;   "Run dired on parent directory of current directory.
;; Find the parent directory either in this buffer or another buffer.
;; Creates a buffer if necessary."
;;   (interactive "P")
;;   (let* ((dir (dired-current-directory))
;;	 (up (file-name-directory (directory-file-name dir))))
;;     (or (dired-goto-file (directory-file-name dir))
;;	;; Only try dired-goto-subdir if buffer has more than one dir.
;;	(and (cdr dired-subdir-alist)
;;	     (dired-goto-subdir up))
;;	(progn
;;	  (if other-window
;;	      (dired-other-window up)
;;	    (progn
;;	      (kill-buffer (current-buffer))
;;	      (dired up))
;;	  (dired-goto-file dir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ diff-with-original
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=diff%20edit%20text
(defun diff-with-original (ediff)
  "Examin diff of current buffer with original file.
	If with prefix, do interactive merge using `ediff-with-original'. "
  (interactive "P")
  (if ediff
      (ediff-with-original)
    ;; simple diff view with diff-mode
    (require 'ediff)
    (let ((diff-buf (get-buffer-create (format "*diff %s*" (buffer-file-name))))
	  (ediff-diff-options "-u") ;; is it your favourite?
	  (tmpfile (ediff-make-temp-file (current-buffer))))
      (save-excursion
	(set-buffer diff-buf)
	(setq buffer-read-only nil)
	(buffer-disable-undo)
	(erase-buffer))
      (ediff-make-diff2-buffer diff-buf
			       (buffer-file-name)
			       tmpfile)
      (delete-file tmpfile)
      (set-buffer diff-buf)
      (if (< (buffer-size) 1)
	  (message "No differences with original file.")
	(condition-case nil
	    (progn
	      (require 'diff-mode)
	      (diff-mode))
	  (error))
	(goto-char 1)
	(pop-to-buffer diff-buf)))))

(defun ediff-with-original ()
  (interactive)
  ;; interactive merge using ediff
  (let ((file buffer-file-name)
	(buf (current-buffer))
	(orig-buf (get-buffer-create (concat "*orig " buffer-file-name "*"))))
    (set-buffer orig-buf)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)
    (insert-file file)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (ediff-buffers orig-buf buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ more easy to switch buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=my%20bury%20buffer
(setq my-ignore-buffer-list
      '("*Help*" "*Compile-Log*" "*Mew completions*" "*Completions*"
	"*Shell Command Output*" "*Apropos*" "*Buffer List*"
	"*Messages*"))

(defun my-visible-buffer (blst)
  (let ((bufn (buffer-name (car blst))))
    (if (or (= (aref bufn 0) ? ) (member bufn my-ignore-buffer-list))
	(my-visible-buffer (cdr blst)) (car blst))))

(defun my-grub-buffer ()
  (interactive)
  (switch-to-buffer (my-visible-buffer (reverse (buffer-list)))))

(defun my-bury-buffer ()
  (interactive)
  (bury-buffer)
  (switch-to-buffer (my-visible-buffer (buffer-list))))

;; (global-set-key [?\C-,] 'my-grub-buffer)
;; (global-set-key [?\C-.] 'my-bury-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ for memo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://namazu.org/~satoru/unimag/1/
;; (defvar memo-file-name "~/memo.txt"
;;   "file name of memo which is using in memo-mode.")

;; (defun memo ()
;;   (interactive)
;;   (let ((add-log-current-defun-function 'ignore)
;; 	(memo-file memo-file-name))
;;     (setq memo-prev-buffer (current-buffer))
;;     (set-buffer (find-file-noselect memo-file))
;;     (add-change-log-entry nil (expand-file-name memo-file))
;;     (set-buffer-modified-p nil)
;;     (define-key change-log-mode-map "\C-xk" 'memo-kill-buffer)))

;; (setq user-full-name "Hideaki Shishido")
;; (setq user-mail-address "hideaki_sh\"at\"ybb.ne.jp")
;; (global-set-key"\C-xm" 'memo)

;; ;; new line and indent
;; (defun memo-newline ()
;;   "New line and indent for memo."
;;   (interactive)
;;   (newline)
;;   (insert "  "))


;; (defvar memo-prev-buffer nil)

;; (defun memo-kill-buffer ()
;;   "Kill buffer with automatic save."
;;   (interactive)
;;   (when (buffer-modified-p)
;;     (save-buffer))
;;   (kill-buffer-current)
;;   (when memo-prev-buffer
;;     (switch-to-buffer memo-prev-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ $B%$%s%G%s%H$7$F!"<!$N9T$K0\F0$9$k(B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;http://kwatch.tripod.co.jp/mule/mule.13.html
;; and some mod.
(defun indent-and-next-line ()
  (interactive)
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point)))
    (if (= beg end)
	(next-line 1)
      (indent-according-to-mode)
      (next-line 1)
      (back-to-indentation))))

;; ;;; M-\ $B$K3d$jEv$F$k(B
;; (define-key global-map "\en" 'indent-and-next-line)
(global-set-key "\M-\\" 'indent-and-next-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ auto-insert $ASC$N%F%s%W%l$B!<$A%H$rWw$C$F$(IA{$A$r$7$h$&(B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://d.hatena.ne.jp/higepon/20080731/1217491155
(require 'autoinsert)

;; $A%F%s%W%l$B!<$A%H$N%G%#%l%/%H%j(B
(setq auto-insert-directory "~/elisp/insert/")

;; $A8w%U%!%$%k$K$h$C$F%F%s%W%l$B!<$A%H$rGP$jLf$($k(B
(setq auto-insert-alist
      (nconc '(
               ("\\.py$" . ["template.py" my-template])
               ("\\.html$" . ["template.html" my-template])
               ;; ("\\.cpp$" . ["template.cpp" my-template])
               ;; ("\\.h$"   . ["template.h" my-template])
               ) auto-insert-alist))
(require 'cl)

;; $A$3$3$,Ms$N$(GKD$A$;Ky(B
(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"    . (lambda () (format "__SCHEME_%s__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
        (progn
          (goto-char (point-min))
          (replace-string (car c) (funcall (cdr c)) nil)))
    template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;;;
;;; apropos
;;;
(defun apropos-next ()
  (interactive)
  (goto-char (1+ (point)))
  (if (re-search-forward "^[^ ]" nil t)
  	  (beginning-of-line)
    (goto-char (point-max))))

(defun apropos-prev ()
  (interactive)
  (beginning-of-line)
  (goto-char (1- (point)))
  (re-search-backward "^[^ ]" nil t)
  (beginning-of-line))

(defadvice apropos-mode (after my-apropos-adv activate)
  (define-key apropos-mode-map "j" 'apropos-next)
  (define-key apropos-mode-map "k" 'apropos-prev))


;;;
;;; my mod
;;;



;;;
;;; end
;;;
(provide 'discrete)

;;; discrete.el ends here
