;; summarye.el -*-emacs-lisp-*- -*- coding:emacs-mule -*-
;; list up matched strings from a buffer, and display them in summary buffer
;; Author: narazaki@InetQ.or.jp (Shuji Narazaki)
;; Version: 2.3.5

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Commentary:
;   Todo:
;; CHANGE LOG
;; narazaki@Inetq.or.jp			Fri Jan 17 13:07:29 1997
;;  prompt for regexp smartly(Ver 2.3)
;; narazaki@csce.kyushu-u.ac.jp		Thu May 11 00:42:38 1995
;;  not always display item at the top of window(Ver 2.2)
;;  add backward scroll function(Ver 2.1)
;; narazaki@csce.kyushu-u.ac.jp		Sat Apr  8 11:00:51 1995
;;  Ver 2.0
;; narazaki@csce.kyushu-u.ac.jp 	Wed Nov 10 16:53:35 1993
;;  Ver 1.0
;; narazaki@hazumi			Sun Nov  8 05:14:49 1992
;;  Ver 0.1

;;; Commentary:

;; Usage:
;; 1. At first set autoload function in your .emacs like this:
;;    (autoload 'se/make-summary-buffer "summarye" nil t)
;; 2. bind se/make-summary-buffer to your favorite key sequence(or menu)
;;    like the following:
;;    (define-key help-map "M" 'se/make-summary-buffer)
;; 3. Invoke it. You will get the summary buffer of current buffer. You
;;    will use it easily, I think.
;; 4. If you want to specify the item pattern, set the value to buffer-local
;;    variable se/item-delimiter-regexp like the following examples. The
;;    value must be either a regular expression string or a list of a list
;;    of a tag string and a regexp string. See examples.
;; 5. And if you want to specify the displayed string in summary buffer,
;;    assign a function to buffer-local variable
;;    se/item-name-constructor-function.

;; Coding memo:
;; * While cluster is an internal structure which index starts from
;;   zero, item means objects user can view like a displayed line or
;;   the corresponding text. Thus every commands do not have cluster
;;   in their names.
;; * In this program, term `face' is used. But it means not face but overlay.
;;; Code:

(defconst se/version "summary-edit Version 2.3.5")
(message se/version)(sit-for 0)

(eval-and-compile
  (require 'cl)
  (or (fboundp 'cl-gethash) (load "cl-extra")))
;;; requires common lisp package
;;; This file uses: dotimes, push, pop, when, unless.
(require 'cl)

;;; requires common lisp extra package
;;; they are used in se/only-once
(autoload 'hash-table-p "cl-extra" nil nil)
(autoload 'cl-gethash "cl-extra" nil nil)
(autoload 'cl-puthash "cl-extra" nil nil)
(if (fboundp 'remove-regexp-in-string)
    (defalias 'se/remove-regexp-in-string 'remove-regexp-in-string)
  (defun se/remove-regexp-in-string (regexp string)
    (cond ((not (string-match regexp string))
	   string)
	  (t (let ((str nil)
		   (ostart 0)
		   (oend (match-beginning 0))
		   (nstart (match-end 0)))
	       (setq str (concat str (substring string ostart oend)))
	       (while (string-match regexp string nstart)
		 (setq ostart nstart)
		 (setq oend (match-beginning 0))
		 (setq nstart (match-end 0))
		 (setq str (concat str (substring string ostart oend))))
	       (concat str (substring string nstart)))))))

(defvar se/incremental-compile-p t
  "If non-nil, each non-compiled formatter is compiled at its
first use.")
(defvar se/display-flush-time 300
  "Time to display overlay temporally. The unit is milisecond.")
(defvar se/*tmp*
  "Bound in se/make-summary-buffer. The first value is nil.
se/only-once bounds it to a hash-table.")
(defvar se/mode-delimiter-alist
  '(((emacs-lisp-mode lisp-interaction-mode lisp-mode)
     (("function" "^(def\\(un\\|method\\|generic\\|macro\\|subst\\) \\([^ \t\n]+\\)\\b")
      ("variable" "^(def\\(var\\|class\\|parameter\\|const\\)\\W+\\([^ \t\n]+\\)\\W"))
     (lambda (beg end category)
       (cond ((member category '("function" "variable"))
	      (se/matched-pattern 2))
	     (t (se/matched-pattern 1)))))
    (bibtex-mode "^@[^{s][^{]*{\\([^,]+\\),$")
    ((c-mode c++-mode cc-mode)
     "^\\(\\*?[A-Za-z_].*(.*\\)$"
     (lambda (beg end category)
       (let ((name (buffer-substring beg end)))
	 (if (string-match "\\([^ 	(]+\\)[ 	]*(.*$" name)
	     (substring name (match-beginning 1) (match-end 0))
	   name))))
    (latex-mode
     "^[ 	]*\\(\\\\chapter\\*?{\\(.*\\)\\|\\\\section\\*?{\\(.*\\)\\|\\\\subsection\\*?{\\(.*\\)\\|\\\\subsubsection\\*?{\\(.*\\)\\|\\\\paragraph{\\(.*\\)$\\)"
     (lambda (beg end category)
       (se/set-face (match-beginning 0) (match-end 0))
       (let ((name (buffer-substring beg end)))
	 (cond ((string-match "\\\\section" name) name)
	       ((string-match "\\\\chapter" name) name)
	       ((string-match "\\\\subsection" name) (concat " " name))
	       ((string-match "\\\\subsubsection" name) (concat "  " name))
	       ((string-match "\\\\paragraph" name) (concat "   " name))))))
    ((ada-mode pascal-mode) "\\(procedure\\|function\\) \\([^ {]+\\)"
     (lambda (beg end category) (se/matched-pattern 1)))
    (perl-mode "^sub \\([^ {]+\\)")
    (prolog-mode
     "^\\(\\w+\\)\\W*("
     (lambda (beg end category)
       (let ((result (se/only-once (se/matched-pattern 1))))
	 (if result (se/set-face (match-beginning 1) (match-end 1)))
	 result)))
    (html-mode
     "^<[Hh][1-6][^>]*>"
     (lambda (beg end category)
       (let ((size (- (char-after (+ beg 2)) ?0)))
	 (if (re-search-forward "</[Hh]" nil t)
	     (concat (make-string (* 2 size) ?\  )
		     (se/remove-regexp-in-string
		      "[\t\n]"
		      (buffer-substring end (match-beginning 0))))))))
    (scheme-mode
     "(define[ \t]+\\([^ \t\n].+\\)"
     (lambda (beg end category)
       (let ((id (se/matched-pattern 1)))
	 (save-excursion
	   (goto-char (match-beginning 0))
	   (concat (make-string (current-column) ?\ ) id)))))
    ((shell-mode comint-mode)
     "^[^#$%>]*[#$%>][ \t]+\\(.+\\)$" ;     "^[^#$%>\n]*[#$%>]\\W+\\(.+\\)$"
     (lambda (beg end category)
       (se/set-face beg end)
       (save-excursion
	 (goto-char (match-beginning 1))
	 (while (looking-at "[ \t]+") (goto-char (match-end 0)))
	 (if (= (point) end)
	     nil
	   (buffer-substring (point) end)))))
    (outline-mode
     outline-regexp
     (lambda (beg end category)
       (goto-char end)
       (end-of-line 1)
       (se/string-subst-char ?\  ?\t
			     (buffer-substring beg (min (+ beg (frame-width))
							(point))))))
    (t nil)				; "^*+[ 	]+\\(.*\\)$"
    )
  "alist of lists of (MODE REGEXP [FORMATTER]), where
MODE is a symbol of mode, or list of symbols of mode;
REGEXP is a regexp string or alist of lists of name (string) and its
regexp (string);
FORMATTER is lambda function (don't enclose in function) that accept
three args as the beginning point (integer) of a matched string to
REGEXP, the end  point (integer) of it, and the name (string) of the
REGEXP; returns string that is used as name of matched string if you
think it is what you want to search exactly. Otherwise returns nil. In
this case, matched string is discarded.")

(defvar se/item-delimiter-regexp 'undefined
  "*delimiter of item. This is a buffer local variable.")
(make-variable-buffer-local 'se/item-delimiter-regexp)
(set-default 'se/summary-delimiter-regexp 'undefined)

(defvar se/item-name-constructor-function nil
  "buffer local function which genereates the appriciate name string to
the item. The function is required accepting three args. First one is the
position(integer or marker) of beginning of the item-delimiter
matched. The second is the end of the item-delimiter matched. The function
is invoked in save-excursion and save-restriction, thun you can move point
anywhere. The third is category(string) or nil.")
(make-variable-buffer-local 'se/item-name-constructor-function)

(defvar se/summary-regexp-history nil)
(defvar se/summary-category nil)
(make-variable-buffer-local 'se/summary-category)
(defvar se/summary-order-by 'position
  "The value must be either position (symbol) or name")
(make-variable-buffer-local 'se/summary-order-by)
(defvar se/default-item-face (if window-system 'secondary-selection 'underline))
(make-variable-buffer-local 'se/default-item-face)

;; mark
(defconst se/summary-item-flags (regexp-quote "*"))
(defconst se/summary-item-flag-regexp (regexp-quote "*"))
(defconst se/mark-character ?\*)

;;;
;;; programming stuff
;;;
(defun se/list-assq-cdr (item list)
  (let (exit)
    (while (and (not exit) list)
      (cond ((eq (car (car list)) item) (setq exit (car list)))
	    ((and (consp (car (car list))) (memq item (car (car list))))
	     (setq exit (car list)))
	    (t (setq list (cdr list)))))
    (cdr exit)))

(defun se/set-item-delimiters-from-mode (mode)
  (let ((regexp-func (or (se/list-assq-cdr mode se/mode-delimiter-alist)
			 (se/list-assq-cdr t se/mode-delimiter-alist)
			 (list "\n\n.+$"))))
    (setq se/item-delimiter-regexp (car regexp-func))
    (when (symbolp se/item-delimiter-regexp)
      (setq se/item-delimiter-regexp (symbol-value se/item-delimiter-regexp)))
    (when (cdr regexp-func)
      (when (and se/incremental-compile-p
		 (not (byte-code-function-p (car (cdr regexp-func)))))
	(setcar (cdr regexp-func) (byte-compile (car (cdr regexp-func)))))
      (setq se/item-name-constructor-function (car (cdr regexp-func))))))

(defsubst se/string-subst-char (new old string)
  (let (index)
    (setq old (regexp-quote (char-to-string old)))
    (while (setq index (string-match old string index))
      (aset string index new)))
  string)

(cond ((featurep 'mule)
       (defalias 'se/string-display-width 'string-width))
      (t 
       (defun se/string-display-width (str) (length str))))

(defsubst se/string-cut-down-to (string width &optional cut-head)
  "make substring of STRING which string-length is WIDTH. If 3rd optional
arg CUT-HEAD is non-nil, the head is ommited. "
  (let ((mc-header 160)			; namely 0xA0
	(diff (- (se/string-display-width string) width))
	(omits 0))
    (if cut-head
	(while (< 0 diff) ; from head
	  (setq omits 1)
	  (while (< 0 diff)
	    (while (< mc-header (aref string omits)) (setq omits (1+ omits)))
	    (setq diff (1- diff)))
	  (setq string (substring string omits))
	  (setq diff (- (se/string-display-width string) width)))
      (while (<= 0 diff) ; from tail
	(setq omits (1- (length string)))
	(while (<= 0 diff)
	  (while (< mc-header (aref string omits)) (setq omits (1- omits)))
	  (setq diff (1- diff)))
	(setq string (substring string 0 omits))
	(setq diff (- (se/string-display-width string) width))))
    string))

(defun se/only-once (string)
  "Return STRING if it's not-yet-matched string.
Note: only use in the extent of se/make-summary-buffer, since the
occurence check uses the hash-table in se/*tmp* ."
  (or (hash-table-p se/*tmp*) (setq se/*tmp* (make-hash-table :test 'equal)))
  (if (cl-gethash string se/*tmp* nil) ; (member match se/*tmp*)
      nil
    (cl-puthash string t se/*tmp*)	; (push match se/*tmp*) ;
    string))

(defsubst se/matched-pattern (beg &optional end)
  (buffer-substring (match-beginning beg) (match-end (or end beg))))

(defconst se/summary-vector 0)
(defconst se/parent-buffer 1)
(defconst se/narrow-in-showing-item 2)
(defconst se/last-shown-cluster 3)
(defconst se/regexp 4)
(defconst se/formatter 5)
(defconst se/show-face 6)
(defconst se/finalize 7)
(defconst se/scroll-when-show 8)
(defconst se/item-documentation 9)
(defconst se/summary-order-list 10)
(defconst se/summary-order-by 11)

(defvar se/*memory-structure* nil
  "List of memories required by the summary edit package and managed by
program automagically. This memory is accessed by both original buffer
and summary buffer. Thus Changing to buffer-local variable is not convenient.")
(make-variable-buffer-local 'se/*memory-structure*)
(defun se/make-se/memory ()
  (vector (list (cons 0 nil)) nil nil nil nil nil nil nil nil nil nil nil))
(defmacro se/ref (name) (` (aref se/*memory-structure* (, name))))
(defmacro se/set (name val) (` (aset se/*memory-structure* (, name) (, val))))

(defun se/ref-summary-vector (&optional category)
  (let ((alist (se/ref se/summary-vector)))
    (cdr (assoc (or category se/summary-category) alist))))
(defalias 'se/cluster-vector 'se/ref-summary-vector)
(defun se/set-summary-vector (category vec)
  (let* ((vec-alist (se/ref se/summary-vector))
	 (cons (assoc category vec-alist)))
    (if cons
	(rplacd cons vec)
      (rplacd vec-alist (cons (cons category vec) (cdr vec-alist))))))

(defun se/cluster-of (nth &optional category)
  "return NTH(zero-base) cluster in se/summary-vector in se/*memory-structure* ."
  (aref (se/ref-summary-vector (or category se/summary-category)) nth))

;;; cluster (vector)		cluster (structure) is created from the
;;;				item (text chank)
;;; that has the following seven fields:.
;;;  postion (integer)		order of this cluster (0-th base)
;;;  beg (marker)		start position of this cluster
;;;  end (or marker nil)	end position of this cluster
;;;  name (string)		the name
;;;  face-block (list)    	list of on face beg end.
;;;  display-string (string)	(cached) displayed string on summary buffer
;;;  position-on-summary(marker)	start position on summary buffer
;;;  marked (string)		operation flag (to delete)

(defmacro se/cluster-position (cluster) (` (aref (, cluster) 0)))
(defmacro se/set-cluster-position (cluster position)
  (` (aset (, cluster) 0 (, position))))
(defmacro se/cluster-name (cluster) (` (aref (, cluster) 1)))
(defmacro se/cluster-beg (cluster) (` (aref (, cluster) 2)))
(defmacro se/cluster-buffer (cluster) 
  (` (and (, cluster) (marker-buffer (aref (, cluster) 2)))))
(defmacro se/cluster-end (cluster) (` (aref (, cluster) 3)))
(defmacro se/set-cluster-end (cluster end) (` (aset (, cluster) 3 (, end))))
(defmacro se/cluster-face-block (cluster) (` (aref (, cluster) 4)))
(defmacro se/set-cluster-face-block (cluster list)
  (` (aset (, cluster) 4 (, list))))
(defmacro se/cluster-display-string (cluster)
  (` (aref (, cluster) 5)))
(defmacro se/set-cluster-display-string (cluster str)
  (` (aset (, cluster) 5 (, str))))
(defmacro se/set-cluster-position-on-summary (cluster point)
  (` (aset (, cluster) 6 (, point))))
(defmacro se/cluster-position-on-summary (cluster)
  (` (aref (, cluster) 6)))
(defmacro se/cluster-marked (cluster)
  (` (aref (, cluster) 7)))
(defmacro se/set-cluster-marked (cluster str)
  (` (aset (, cluster) 7 (, str))))
(defun se/make-cluster (nth name beg &optional end)
  "NTH BEG END &optional NAME"
  (or (markerp beg) (error "a cluster is not bound to a buffer"))
  (vector nth name beg end nil nil nil nil))
(defun se/update-cluster (cluster nth name beg &optional end face string mark)
  "NTH BEG END &optional NAME"
  (aset cluster 0 nth)
  (aset cluster 1 name)
  (if (markerp beg) (aset cluster 2 beg)
    (error "a cluster is not bound to a buffer"))
  (aset cluster 3 end)
  (if face (aset cluster 4 face))
  (if string (aset cluster 5 string))
  (if mark (aset cluster 7 mark))
  cluster)

(defun se/map-on-cluster (function)
  (let ((cluster-vector (se/cluster-vector)))
    (dotimes (index (length cluster-vector))
      (funcall function (aref cluster-vector index)))))

(defun se/make-summary-vector (category widen &optional old-vec)
  (save-excursion
    (save-restriction
      (if widen (widen))
      (let ((vec-index 0)
	    (case-fold-search (se/case-fold category))
	    list tmp number-of-item vec)
	(goto-char (point-min))
	(if (vectorp old-vec)
	    (while
		(setq tmp (se/search-next-cluster
			   category
			   (if (< vec-index (length old-vec))
			       (aref old-vec vec-index))))
	      (push tmp list)
	      (setq vec-index (1+ vec-index)))
	  (while (setq tmp (se/search-next-cluster category))
	    (push tmp list)))
	(setq number-of-item (length list))
	(if (= number-of-item 0) (error "No item found"))
	(setq vec (make-vector number-of-item nil))
	(dotimes (index number-of-item)
	  (aset vec (- number-of-item (1+ index)) (pop list)))
	(dotimes (index number-of-item)
	  (se/set-cluster-position (aref vec index) index))
	;; initialize end
	(dotimes (index (1- number-of-item))
	  (se/set-cluster-end (aref vec index)
			      (se/cluster-beg (aref vec (1+ index)))))
	(se/set-cluster-end (aref vec (1- number-of-item)) (point-max))
	(se/set-summary-vector category vec)))))

(defun se/get-delimiter (category)
  "return delimiter (regexp string) of CATEGORY (string)"
  (let ((regexp (or (se/ref se/regexp) se/item-delimiter-regexp)))
    (cond ((stringp regexp) regexp)
	  ((and (consp regexp) (symbolp (cdr regexp))) (car regexp))
	  (t (car (cdr (assoc category regexp)))))))

(defun se/case-fold (category)
  "return  case-fold search flag(nil/t) for CATEGORY (string)"
  (let ((regexp (or (se/ref se/regexp) se/item-delimiter-regexp)))
    (cond ((stringp regexp) nil)
	  ((and (consp regexp) (symbolp (cdr regexp))) (cdr regexp))
	  (t (cdr (cdr (assoc category regexp)))))))

(defun se/get-non-delimiter (category)
  (and (listp se/item-delimiter-regexp)
       (car (cdr (cdr (assoc category se/item-delimiter-regexp))))))

(defun se/search-next-cluster (category &optional cluster)
  "search the next cluster CATEGORY (string) from current point (not
passed as an arg)."
  (let (tmp)
    (while (not (or (setq tmp (se/search-next-cluster-aux category cluster))
		    (eobp))))
    tmp))

(defvar *the-cluster*)
(defun se/search-next-cluster-aux (category &optional cluster)
  "CATEGORY (string)"
  (if (re-search-forward (se/get-delimiter category) (point-max) t)
      (let ((name nil)
	    (end (match-end 0))
	    (beg (match-beginning 0))
	    (marker nil))
	(when (se/get-non-delimiter category)
	  (goto-char beg)
	  (re-search-forward (se/get-delimiter category) (point-max) t))
	(if (and cluster (markerp (se/cluster-beg cluster)))
	    (setq marker (se/cluster-beg cluster))
	  (setq marker (make-marker)))
	(set-marker marker beg)
	(goto-char end)
	(or cluster (setq cluster (se/make-cluster 0 nil marker)))
	(let ((*the-cluster* cluster))
	  (if (se/ref se/formatter)
	      ;;(narrow-to-region beg (match-end 0))
	      (setq name (funcall (se/ref se/formatter)
				  beg (match-end 0) category))
	    (let ((mbeg (or (match-beginning 1) (match-beginning 0)))
		  (mend (or (match-end 1) (match-end 0))))
	      (se/set-face mbeg mend)
	      (setq name (se/string-subst-char ?\  ?\	 (buffer-substring mbeg mend))))))
	(if name (se/update-cluster cluster 0 name marker)))
    (goto-char (point-max))
    nil))

(defun se/summary-buffer-name (category string)
  (format "%s-in-%s" (or category "Items") string))

;; [command/program protocol]
;;;###autoload
(defun se/make-summary-buffer-mouse (e)
  (interactive "e")
  (let (cat-name)
    (let ((cat-name nil))
      (when (eq se/item-delimiter-regexp 'undefined)
	(se/set-item-delimiters-from-mode major-mode))
      (cond ((stringp se/item-delimiter-regexp)
	     (se/make-summary-buffer cat-name))
	    ((consp se/item-delimiter-regexp)
	     (setq cat-name
		   (x-popup-menu
		    e
		    (list "Smmary Category"
			  (cons "Summary Category"
				(mapcar
				 (lambda (name+regex)
				   (cons (car name+regex) (car name+regex)))
				 se/item-delimiter-regexp)))))
	    (if cat-name (se/make-summary-buffer cat-name)))))))

(defun se/make-summary-buffer (&optional cat-name widen/interactive memory regexp
					 formatter finalize)
  "Make summary buffer. If prefix arg is larger than 1, search items from
whole buffer. Otherwise, and if buffer is narrowed, search items from
narrowed region. If cat-name is t, set regexp interactively.
If prefix arg is minus and invoked interactively, then you can set regexp
interactively.
"
  (interactive
   (list
    (progn
      (when (eq se/item-delimiter-regexp 'undefined)
	(se/set-item-delimiters-from-mode major-mode))
      (cond ((stringp se/item-delimiter-regexp) nil)
	    ((null se/item-delimiter-regexp) t)
	    (t (completing-read "Category or RETURN(for new regexp): "
				se/item-delimiter-regexp nil nil))))
    (prefix-numeric-value current-prefix-arg)))
  ;; If non-interactive invocation is done before interactive one,
  ;; we must check the value of se/item-delimiter-regexp at first.
  (when (eq se/item-delimiter-regexp 'undefined)
    (se/set-item-delimiters-from-mode major-mode))
  (if (or (eq cat-name t) ; (and (stringp se/item-delimiter-regexp))
	  (and (not (stringp se/item-delimiter-regexp))
	       (not regexp)
	       (not memory)
	       (not (assoc cat-name se/item-delimiter-regexp)))
	  (and widen/interactive	; prefix arg
	       (< widen/interactive 0)
	       (interactive-p))
	  (equal cat-name ""))		; completing-read
      (setq cat-name nil
	    formatter t			; see (se/set se/formatter ...)
	    regexp
	    (read-from-minibuffer "Summary of items matching regexp: "
				  (car se/summary-regexp-history)
				  nil
				  nil
				  'se/summary-regexp-history)))
  (let* ((se/*tmp*)
	 (se/memory)
	 (summary-buffer-name (se/summary-buffer-name cat-name (buffer-name)))
	 (widen-p (and (numberp widen/interactive)
		       (< 1 (abs widen/interactive))))
	 (buf (get-buffer-create summary-buffer-name))
	 (parent-buffer (current-buffer)))
    (let ((se/*memory-structure* (se/make-se/memory)))
      (se/set se/parent-buffer (current-buffer))
      (se/set se/regexp
	      (if memory (aref memory se/regexp)
		(or regexp se/item-delimiter-regexp)))
      (se/set se/formatter
	      (if memory (aref memory se/formatter) ; specail branch
		(cond ((eq formatter t) nil)
		      (formatter formatter)
		      (t se/item-name-constructor-function))))
      (se/set se/finalize (if memory (aref memory se/finalize) finalize))
      (se/make-summary-vector cat-name (and widen/interactive widen-p)
			      (if memory
				  (aref memory se/summary-vector)
				(se/ref-summary-vector cat-name)))
      (setq se/memory se/*memory-structure*))
    ;; change buffer
    (se/pop-to-buffer buf)
    (summary-edit-summary-mode)
    (setq se/*memory-structure* se/memory)
    (setq se/summary-category cat-name)
    (se/initialize-local-variables buf cat-name)
    (se/update-summary-buffer)
    (and (eq se/summary-order-by 'name) (se/sort-summary-by-name))
    (if (se/ref se/show-face) (se/set-all-item-faces 'on))
    (goto-char (point-min))
  (and (se/ref se/finalize) (funcall (se/ref se/finalize)))))

(defun se/insert-cluster-here (cluster newline width lineformatter cachedp)
  (if newline (insert "\n"))
  (se/set-cluster-position-on-summary cluster (point))
  (insert (se/summary-display-format cluster width lineformatter cachedp))
  (put-text-property (save-excursion (beginning-of-line)
				     (forward-char 9)
				     (point))
		     (save-excursion (end-of-line) (point))
		     'mouse-face
		     'highlight))

(defun se/update-summary-buffer (&optional buf lineformatter cachedp)
  (save-excursion
    (and buf (pop-to-buffer buf))
    (let ((buffer-read-only nil)
	  (summary (se/ref-summary-vector se/summary-category))
	  (list (se/ref se/summary-order-list))
	  (slist (se/ref se/summary-order-list))
	  (width (frame-width)))
      (erase-buffer)
      (if (or (null list) (eq se/summary-order-by 'position))
	  (dotimes (index (length summary))
	    (se/insert-cluster-here (aref summary index) (< 0 index)
				    width lineformatter cachedp))
	(se/insert-cluster-here (pop list) nil width lineformatter cachedp)
	(while list
	  (se/insert-cluster-here (pop list) t width lineformatter cachedp))))
    (shrink-window-if-larger-than-buffer)))

(defun se/summary-display-format (cluster width &optional lineformatter cachedp)
  (let* ((str (se/cluster-display-string cluster))
	 (line-format "%c   %3d: %s %3s")
	 (name-width (- width 14)) ; 3 + 3 + 2 + 1 + 3 = 12
	 (lin (cond ((null lineformatter)
		     (save-excursion
		       (set-buffer (se/cluster-buffer cluster))
		       (save-restriction
			 (widen)
			 (format "%3d"
				 (count-lines (se/cluster-beg cluster)
					      (se/cluster-end cluster))))))
		    ((stringp lineformatter) (format "%3s" lineformatter))
		    ((or (byte-code-function-p lineformatter)
			 (symbolp lineformatter))
		     (format "%3s" (funcall lineformatter cluster))))))
    (unless (and cachedp str)
      (setq str (se/string-cut-down-to (se/cluster-name cluster) name-width))
      (setq str (concat str (make-string (- name-width 
					    (se/string-display-width str)) ?\ )))
      (se/set-cluster-display-string cluster str))
    (format line-format
	    (or (se/cluster-marked cluster) ? )
	    (1+ (se/cluster-position cluster)) str lin)))

(defun se/shortcut-menu (event)
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (sit-for 0)
  (let ((op (x-popup-menu
	     event
	     (list "summary menu"
		   (list "summary menu"
			 '("Show" . se/show-current-item)
			 '("Go to" . se/goto-current-item)
			 '("Toggle face" . se/toggle-this-item-face)
			 '("--" . nil)
			 '("Mark" . se/mark-current-item)
			 '("Unmark" . se/unmark-current-item)
			 '("Merge to next" . se/merge-items)
			 '("Delete" . se/delete-from-summary)
			 '("Clear all mark" . se/unmark-all)
			 '("--" . nil)
			 '("Sort by name" . se/sort-summary-by-name)
			 '("Sort by position" . se/sort-summary-by-position)
			 '("Make unique" . se/unique)
			 '("Toggle all faces" . se/toggle-all-item-faces)
			 '("Clear all faces" . se/clear-all-faces)
			 '("Remake buffer" . se/remake-summary)
			 '("Item definition" . se/show-item-documentation))))))
    (and op (call-interactively op))))

(defun summary-edit-summary-mode ()
  "mode for displaying other buffer's items as summary.
\\[se/show-current-item]	display the item, and scroll up if already shown.
\\[se/show-current-item-backward]	display the item, and scroll down if already shown.
\\[se/jump-to/current-item]	display/scroll current or NUMBER item.
\\[se/forward-line]	go to next line and display the item in other window.
\\[se/previous-line]	go to previous line and display the item in other window.
\\[se/goto-current-item]	go to the current item in original buffer.
\\[se/delete-from-summary]	delete current line from summary buffer.
\\[se/unique]	sort and `uniq' items.
\\[se/remake-summary]	remake summary buffer(this buffer).
\\[se/jump-to-item-of]	jump to the Nth item and display it.
\\[se/quit-and-go-parent-buffer]	erase the summary buffer and go to original buffer.
\\[se/merge-items]		merge current item and the next.
\\[se/mark-current-item]	mark the current item.
\\[se/unmark-current-item]	unmark the current item.
\\[se/unmark-all]		unmark all items.
\\[se/narrow-in-showing-item]	toggle the display style of item.
\\[se/sort-summary-by-position]	sort summary by the position.
\\[se/sort-summary-by-name]	sort summary by item name.
\\[se/toggle-this-item-face]	toggle face of the current item.
\\[se/toggle-all-item-faces]	toggle faces of all items.
\\[se/clear-all-faces]	clear all faces(overlay exactly) even if genearated by other package.
\\[se/show-item-documentation]		about regexp used to generate this summary.
"
  (interactive)
  (let ((old-summary-order se/summary-order-by)
	(ml mode-line-buffer-identification))
    (kill-all-local-variables)
    (use-local-map summary-edit-mode-map)
    (make-local-variable 'se/summary-category)
    (make-local-variable 'se/summary-order-by)
    (if old-summary-order (setq se/summary-order-by old-summary-order))
    (setq buffer-read-only t)
    (setq major-mode 'summary-edit-summary-mode)
    (setq mode-name "Item-Summary")
    (setq mode-line-buffer-identification ml))
  (run-hooks 'summary-edit-summary-mode-hook))

(defun se/initialize-local-variables (buffer category)
  (save-excursion
    (set-buffer buffer)
    (setq se/summary-category category)))

(defun se/current-cluster (&optional category)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
	 (concat "^" se/summary-item-flag-regexp "? +\\([0-9]+\\):")
	 nil t)
	(se/cluster-of (1- (string-to-int (se/matched-pattern 1))) category))))

(defun se/current-cluster-number ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
	 (concat "^" se/summary-item-flag-regexp "? +\\([0-9]+\\):")
	 nil t)
	(1- (string-to-int (se/matched-pattern 1))))))

(defun se/current-item-number ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
	 (concat "^" se/summary-item-flag-regexp "? +\\([0-9]+\\):")
	 nil t)
	(string-to-int (se/matched-pattern 1)))))

;; * display commands
(defun se/goto-current-item ()
  (interactive)
  (se/set se/last-shown-cluster nil)
  (se/show-cluster (se/current-cluster)))

(defun se/show-current-item ()
  (interactive)
  (beginning-of-line)
  (se/show-cluster (se/current-cluster) (current-buffer)))

(defun se/show-current-item-backward ()
  (interactive)
  (beginning-of-line)
  (se/show-cluster (se/current-cluster) (current-buffer) 'backward))

(defun se/mouse-show-current-item (event)
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (se/show-current-item)
  (select-window (posn-window (event-end event))))

(defun se/jump-to/current-item (nth)
  (interactive "P")
  (if nth
      (se/jump-to-item-of (prefix-numeric-value nth))
    (se/show-current-item)))

(defun se/jump-to-item-of (nth)
  (interactive "P")
  (or nth
      (setq nth (string-to-int (read-from-minibuffer "Item number to jump: "))))
  (se/jump-to-cluster-of (1- nth)))

(defun se/jump-to-cluster-of (nth)
  (goto-char (se/cluster-position-on-summary (se/cluster-of nth)))
  (se/show-cluster (se/cluster-of nth) (current-buffer)))

(defmacro se/walk-frame-for-buffer (buffer &rest body)
  (` (let ((buffer (, buffer))
	   (cwindow (selected-window))
	   (cframe (selected-frame)))
       (mapcar
	(function
	 (lambda (frame)
	   (select-frame frame)
	   (walk-windows
	    (function
	     (lambda (win)
	       (when (eq (window-buffer win) buffer)
		 (select-window win)
		 (,@ body))))
	    nil 'only)))
	(frame-list))
       (select-frame cframe)
       (select-window cwindow))))

;(macroexpand '(se/walk-frame-for-buffer (current-buffer) (test)))
;(put 'se/walk-frame-for-buffer 'lisp-indent-function 1)
(defun se/show-cluster (cluster &optional point-continuation backward-p)
  (let ((last-item)
	(mem se/*memory-structure*)
	(already-shown-p (eq (se/ref se/last-shown-cluster) cluster))
	(original-buffer (current-buffer)))
    (when (se/pop-to-buffer (se/cluster-buffer cluster))
      (let ((se/*memory-structure* mem)
	    (buffer (current-buffer)))
	(if already-shown-p
	    (se/walk-frame-for-buffer buffer
 	      (if backward-p
		  (condition-case beg
		      (scroll-down)
		    (begininng-of-buffer
		     (message "End of item")
		     (set-window-start (get-buffer-window (current-buffer))
				       (se/cluster-beg cluster))))
		(condition-case end
		    (scroll-up)
		  (end-of-buffer
		   (message "End of item")
		   (set-window-start (get-buffer-window (current-buffer))
				     (se/cluster-beg cluster))))))
	  (se/walk-frame-for-buffer buffer
	    (widen)
	    (goto-char (se/cluster-beg cluster))
	    (if (and 'will-be-replaced-by-a-slot-in-memory-structure
		     (se/cluster-beg cluster)
		     (se/cluster-end cluster)
		     (pos-visible-in-window-p (se/cluster-beg cluster))
		     (pos-visible-in-window-p (se/cluster-end cluster)))
		'nothing
	      (set-window-start (get-buffer-window (current-buffer))
				(save-excursion
				  (beginning-of-line
				   (se/ref se/scroll-when-show))
				  (point))))
	    (if (se/ref se/narrow-in-showing-item)
		(narrow-to-region (point) (se/cluster-end cluster)))
	    (se/set se/last-shown-cluster cluster))))
      ;; the following sexp must be out of let of se/*memory-structure*
      (let ((sec (if (numberp se/display-flush-time) 0 1))
	    (msec (and (numberp se/display-flush-time) se/display-flush-time)))
	(if point-continuation (se/pop-to-buffer point-continuation))
	(when (and (not already-shown-p)
		   (sit-for sec msec))		; redisplay but without overlay
	  (se/set-cluster-face cluster 'toggle)
	  (if msec (sit-for 0 msec))
	  (se/set-cluster-face cluster 'toggle))))))

(defun se/pop-to-buffer (buffer)
  "Return BUFFER (as non-nil value) when BUFFER exists. Otherwise nil."
  (if (and (bufferp buffer) (buffer-name buffer))
      (progn
	(if (not (get-buffer-window buffer 'visible))
	    (pop-to-buffer buffer)
	  (select-frame (window-frame (or (get-buffer-window buffer)
					  (get-buffer-window buffer 'visible))))
	  (pop-to-buffer buffer))
	buffer)
    (message "Parent buffer does not exist now.")
    nil))

(defun se/remake-summary ()
  (interactive)
  (let ((category se/summary-category)
	(mem se/*memory-structure*)
 	(nth (se/current-cluster-number)))
    (message "remaking...")
    (set-buffer (se/ref se/parent-buffer))
    (se/make-summary-buffer category nil mem)
    (let ((length (length (se/cluster-vector))))
      (when (and (numberp nth) (plusp length))
	(se/jump-to-cluster-of (min nth (1- length)))))
    (message "remaking...done")))

(defun se/previous-line (arg)
  (interactive "p")
  (let ((pos (point)))
    (forward-line (- arg))
    (beginning-of-line)
    (if (= (point) pos)
	(message "Beginning of buffer")
      (se/show-current-item))))

(defun se/forward-line (arg)
  (interactive "p")
  (let ((pos (point)))
    (forward-line arg)
    (beginning-of-line)
    (if (= (point) pos)
	(message "End of buffer")
      (se/show-current-item))))

(defun se/narrow-in-showing-item ()
  "Toggle item display style."
  (interactive)
  (save-excursion
    (se/set se/narrow-in-showing-item
	    (not (se/ref se/narrow-in-showing-item)))
    (if (se/ref se/narrow-in-showing-item)
	(message "Item is displayed with narrowing.")
      (message "All text is displayed when displaying item.")))
    (se/show-current-item))

;; * mark
(defun se/mark-current-item (&optional char)
  (interactive)
  (or char (setq char se/mark-character))
  (se/set-cluster-marked (se/current-cluster se/summary-category) char)
  (let (buffer-read-only)
    (beginning-of-line)
    (delete-char 1)
    (insert (char-to-string char)))
  (se/forward-line 1))

(defun se/unmark-current-item ()
  (interactive)
  (se/set-cluster-marked (se/current-cluster se/summary-category) nil)
  (let (buffer-read-only)
    (beginning-of-line)
    (delete-char 1)
    (insert " "))
  (se/previous-line 1))

(defun se/unmark-all ()
  (interactive)
  (let ((current (se/current-cluster-number)))
    (se/map-on-cluster
     (function (lambda (cluster) (se/set-cluster-marked cluster nil))))
    (se/update-summary-buffer nil nil 'already-cached)
    (se/jump-to-cluster-of current)))

;; * sort, order control
(defun se/sort-cluster (predicate)
  (let* ((vec (se/ref-summary-vector se/summary-category))
	 (length (length vec))
	 (last (1- length))
	 (tmp nil)
	 (new nil))
    (dotimes (index length) (push (aref vec (- last index)) tmp))
    (setq new (sort tmp predicate))
    (se/set se/summary-order-list new)))

(defun se/sort-summary-by-name ()
  (interactive)
  (let ((buffer-read-only nil)
	(nth (se/current-cluster-number)))
    ;; We can use both of the following two methods.
    ;; 1. string on buffer based searach
    ;; 2. cluster structure based search
    ;; The first seems faster than the second.
    ;; But to separate displayed style from internal data structure,
    ;; I dediced to use the second method.
    (se/sort-cluster
      (function
       (lambda (c1 c2) (string< (se/cluster-name c1) (se/cluster-name c2)))))
    (setq se/summary-order-by 'name)
    (se/update-summary-buffer nil nil 'already-cached)
    (se/jump-to-cluster-of nth)))

(defun se/sort-summary-by-position ()
  (interactive)
  (let ((buffer-read-only nil)
	(nth (se/current-cluster-number)))
    (se/sort-cluster
      (function
       (lambda (c1 c2) (string< (se/cluster-name c1) (se/cluster-name c2)))))
    (setq se/summary-order-by 'position)
    (se/update-summary-buffer nil nil 'already-cached)
    (se/jump-to-cluster-of nth)))

(defun se/sort-item-by-summary ()
  (interactive)
  (yes-or-no-p "You want to sort the text really?"))

(defun se/quit-and-go-parent-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (if (se/pop-to-buffer (se/cluster-buffer (se/ref se/last-shown-cluster)))
	(progn (widen)
	       (delete-other-windows))
      (switch-to-buffer nil))		; Wed Apr 19 13:41:44 1995
    (kill-buffer buf)))

(defun se/unique ()
  (interactive)
  (let ((buffer-read-only nil)
	(cat se/summary-category)
	(nth (se/current-cluster-number)))
    (or (eq se/summary-order-by 'name) (se/sort-summary-by-name))
    (let* ((vec (se/ref-summary-vector cat))
	   (items (se/unique-cluster-number-list))
	   (num (length items))
	   (index 0)
	   (new (make-vector num nil)))
      (while items
	(aset new index (aref vec (car items)))
	(setq items (cdr items)
	      index (1+ index)))
      (se/numbering-summary-vector new)
      ;(se/face-off)
      (se/set-summary-vector cat new)
      (se/set se/summary-order-list nil)
      (se/update-summary-buffer nil "" t))))

(defun se/numbering-summary-vector (vec)
  (dotimes (index (length vec))
    (se/set-cluster-position (aref vec index) index)))

(defun se/delete-from-summary (&optional list)
  (interactive)
  (let ((cat se/summary-category)
	(buf (current-buffer)))
    (or list
	(setq list (let ((tmp))
		    (se/map-on-cluster
		     (function (lambda (c)
				 (if (se/cluster-marked c)
				     (push (se/cluster-position c) tmp)))))
		    tmp))
	(setq list (se/current-cluster-number)))
    (if (numberp list) (setq list (list list)))
    (beginning-of-line)
    (se/delete-clusters-from-summary list cat buf)
    (se/jump-to-cluster-of
     (min (max 0 (1- (length (se/ref-summary-vector cat)))) (car list)))))

(defun se/merge-items (&optional num)
  (interactive)
  (let ((cat se/summary-category)
	cluster next)
    (or num (setq num (se/current-cluster-number)))
    (setq cluster (se/cluster-of num))
    (or (setq next (se/cluster-of (1+ num))) (error "No next item to merge"))
    (se/set-cluster-end cluster (se/cluster-end next))
    (and (se/cluster-face-block cluster)
	 (se/cluster-face-block next)
	 (let* ((curr-attr (se/cluster-face-block cluster))
		(next-attr (se/cluster-face-block next))
		(beg (nth 2 curr-attr))
		(newend (nth 3 next-attr)))
	   (= (nth 3 curr-attr) (nth 2 next-attr))
	   (setcdr (nthcdr 2 curr-attr) (nthcdr 3 next-attr))))
    (se/delete-from-summary (1+ num))
    (se/jump-to-cluster-of num)
    (if (car (se/cluster-face-block cluster))
	(se/set-cluster-face cluster 'on))))

(defun se/unique-cluster-number-list ()
  (save-excursion
    (goto-char (point-min))		; [Thu Feb 22 12:43:47 1996]
    (let ((list (list (se/current-cluster-number)))
	  (cur 0))
      (while (not (eobp))
	(setq cur (se/current-cluster-number))
	(if (not (string= (se/cluster-display-string (se/cluster-of (car list)))
			  (se/cluster-display-string (se/cluster-of cur))))
	    (push cur list))
	(forward-line 1))
      (nreverse list))))

(defun se/delete-clusters-from-summary (list cat buf)
  (let* ((vec (se/ref-summary-vector cat))
	 (len (- (length vec) (length list)))
	 (index 0)
	 (last 0)
	 (cluster)
	 (new (make-vector len nil)))
    (dolist (nth list) (se/set-cluster-face (se/cluster-of nth) 'off))
    (while (< last len)
      (setq cluster (aref vec index))
      (unless (memq (se/cluster-position cluster) list)
	(aset new last cluster)
	(setq last (1+ last)))
      (setq index (1+ index)))
    (when (se/ref se/summary-order-list)
      (se/set se/summary-order-list (cons nil (se/ref se/summary-order-list)))
      (let ((rest (se/ref se/summary-order-list)))
	(while (cdr rest)
	  (if (member (se/cluster-position (car (cdr rest))) list)
	      (setcdr rest (cdr (cdr rest))))
	  (pop rest)))
      (se/set se/summary-order-list (cdr (se/ref se/summary-order-list))))
    ;; rebuild number sequence
    (se/numbering-summary-vector new)
    (se/set-summary-vector cat new)
    (se/update-summary-buffer nil nil t)
    (and (eq se/summary-order-by 'name)
	 (not (se/ref se/summary-order-list))
	 (se/sort-summary-by-name))))

;;; * face
(defun se/face-on-region (attr beg end)
  (let ((flag (buffer-modified-p))
	(overlay (make-overlay beg end)))
    (overlay-put overlay 'face attr)
    (overlay-put overlay 'summarye t)
    (set-buffer-modified-p flag)))

(defun se/face-off-region (attr beg end)
  (let ((flag (buffer-modified-p))
	(lstart 0))
    (while (and beg (> beg lstart) (< beg end))
      (mapcar (function (lambda (ovr)
			  (and (overlay-get ovr 'summarye)
			       (delete-overlay ovr))))
	      (overlays-at beg))
      (setq lstart beg beg (next-overlay-change beg)))
    (set-buffer-modified-p flag)))

;; [initialize protocol]
(defun se/set-face (beg end &optional attr on)
  "set face memory on region BEG END as ATTRIBUTE, and
use(display) it now if ON is non-nil. The type of BEG and END is either
integer or marker. The type of ATTR is symbol. Its default value is bold."
  (if (numberp beg) (setq beg (set-marker (make-marker) beg)))
  (if (numberp end) (setq end (set-marker (make-marker) end)))
  (or attr (setq attr se/default-item-face))
  ;; *the-cluster* is defined in search-next-cluster-aux
  (se/set-cluster-face-block *the-cluster* (list on attr beg end))
  (if on
      (save-excursion
	(if (markerp beg) (set-buffer (marker-buffer beg)))
	(se/face-on-region attr beg end))))

(defun se/set-cluster-face (cluster flag)
  (if (eq flag 'toggle)
      (se/set-cluster-face cluster
			   (if (car (se/cluster-face-block cluster)) 'off 'on))
    (let ((val (cond ((eq flag 'on) t)
		     ((eq flag 'off) nil))))
      (if (se/cluster-face-block cluster)
	  (rplaca (se/cluster-face-block cluster) val)
	(se/set-cluster-face-block
	 cluster (list val se/default-item-face (se/cluster-beg cluster)
		       (se/cluster-end cluster))))
      (save-excursion
	(set-buffer (se/cluster-buffer cluster))
	(apply (if val 'se/face-on-region 'se/face-off-region)
	       (cdr (se/cluster-face-block cluster)))))))

(defun se/toggle-this-item-face ()
  (interactive)
  (se/set-cluster-face (se/current-cluster) 'toggle))

(defun se/clear-all-faces ()
  (interactive)
  (save-excursion
    (set-buffer (se/ref se/parent-buffer))
    (se/face-off-region nil (point-min) (point-max))))

(defun se/toggle-all-item-faces ()
  (interactive)
  (se/set-all-item-faces 'toggle))

(defun se/set-all-item-faces (flag)
  (if (eq flag 'toggle)
      (se/set-all-item-faces (if (se/ref se/show-face) 'off 'on))
    (let ((mes (cond ((eq flag 'on) "Face on...")
		     ((eq flag 'off) "Face off...")))
	  (val (eq flag 'on)))
      (message mes)
      (se/set se/show-face val)
      (dotimes (i (length (se/ref-summary-vector se/summary-category)))
	(se/set-cluster-face (se/cluster-of i) flag))
      (message (concat mes " done.")))))

;; * help system
(defun se/show-item-documentation ()
  "Display item-documentation of current summary."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let ((cat se/summary-category))
      (princ "Category Name:\n")
      (princ (or se/summary-category "anonymous"))
      (terpri)
      (princ "Regexp:\n")
      (print (or (se/get-delimiter cat) "?"))
      (terpri)
      (when (stringp (se/ref se/item-documentation))
	(princ "Documentation:\n")
	(princ (se/ref se/item-documentation))))
    (print-help-return-message)))

;; * key bindings
(eval-and-compile
  (defvar summary-edit-mode-map (make-sparse-keymap)))
(dotimes (index 10)
  (define-key summary-edit-mode-map (int-to-string index) 'digit-argument))
(defvar summary-edit-mode-key-bindings
  (list '("n" se/forward-line)
	'([down] se/forward-line)
	'("p" se/previous-line)
	'([up] se/previous-line)
	'("[?\177]" se/show-current-item-backward)
	'("\177" se/show-current-item-backward)
	'([delete] se/show-current-item-backward)
	'([backspace] se/show-current-item-backward)
	'("" se/show-current-item-backward)
	'("f" se/goto-current-item)
	'(" " se/show-current-item)
	'("b" se/show-current-item-backward)
	'("m" se/mark-current-item)
	'("u" se/unmark-current-item)
	'("M" se/merge-items)
	'("d" se/delete-from-summary)
	'("g" se/remake-summary)
	'("q" se/quit-and-go-parent-buffer)
	'("j" se/jump-to-item-of)
	'("\C-m" se/jump-to/current-item)
	'("\C-c\C-dn" se/narrow-in-showing-item)
	'("\C-c\C-sn" se/sort-summary-by-name)
	'("\C-c\C-sp" se/sort-summary-by-position)
	'("\C-c\C-ss" se/sort-item-by-summary)
	'("a" se/toggle-this-item-face)
	'("A" se/toggle-all-item-faces)
	'("C" se/clear-all-faces)
	'("h" se/show-item-documentation)
	(list [menu-bar summary] (cons "Summary" (make-sparse-keymap "Summary")))
	'([menu-bar summary item-documentation]
	  ("Item definition" . se/show-item-documentation))
	'([menu-bar summary delete]
	  ("Delete it from summary" . se/delete-from-summary))
	'([menu-bar summary merge]
	  ("Merge current item and the next" . se/merge-items))
	'([menu-bar summary unique] ("Make items unique" . se/unique))
	'([menu-bar summary show] ("Show it" . se/show-current-item))
	'([menu-bar summary go] ("Go to it" . se/goto-current-item))
	'([menu-bar summary remake] ("Remake summary" . se/remake-summary))
	'([menu-bar summary mark] ("Mark" . se/mark-current-item))
	'([menu-bar summary unmark] ("Unmark" . se/unmark-current-item))
	'([menu-bar summary sort-by-position]
	  ("Sort by position" . se/sort-summary-by-position))
	'([menu-bar summary sort-by-name]
	   ("Sort by name" . se/sort-summary-by-name))
	'([menu-bar summary narrow-display]
	  ("Toggle (not) narrowed display" . se/narrow-in-showing-item))
	'([menu-bar summary toggle-face]
	  ("Toggle face " . se/toggle-this-item-face))
	'([menu-bar summary clear-face]
	  ("Clear all faces" . se/clear-all-faces))
	'([menu-bar summary toggle-face-all]
	  ("Toggle faces of all items" . se/toggle-all-item-faces))
	'([mouse-2] se/mouse-show-current-item)
	'([down-mouse-3] se/shortcut-menu)))
(let ((list summary-edit-mode-key-bindings))
  (while list (apply (function define-key) summary-edit-mode-map (pop list))))

(provide 'summarye)

;; Local Variables:
;; buffer-file-coding-system: emacs-mule
;; End:
;; summarye.el ends here