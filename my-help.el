;;; my-help.el
;;;
;;; refs
;;; http://hpcgi1.nifty.com/spen/index.cgi?ZaurusSL%2dC3000%2f%bd%e9%b4%fc%c0%df%c4%ea%2femacs%a4%bd%a4%ce%a3%b3#i0
;;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?%BC%C1%CC%E4%BD%B8%2F31
;;;
;;;
;;; * helpfile location: my-help-directory variable
;;;
;;; * helpfile name
;;;  global:     myhelp.txt
;;;  each mode:  xxx-mode.txt
;;;

(defvar my-help-directory "~/info")

(defun my-help ()
  (interactive)
  (let ((filename
	 (expand-file-name
	  (format
	   "%s%s"
	   major-mode
	   ".txt")
	  my-help-directory))
	(default-file
	  (expand-file-name
	   (format
	    "%s%s"
	    "myhelp"
	    ".txt")
	   my-help-directory)))
    (with-output-to-temp-buffer "*myhelp*"
      (set-buffer standard-output)
      (if (file-exists-p filename)
	  (insert-file-contents filename))
      (goto-char (point-max))
      (insert "==== myhelp ================================================\n\n")
      (if (file-exists-p default-file)
	  (insert-file-contents default-file))
      (goto-char (point-min))
      )))

;;; keybind
;; (global-set-key "\M-z" 'my-help)
