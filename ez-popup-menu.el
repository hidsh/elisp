;;
;; Filename: ez-popup-menu.el
;; Last modified: Mon Feb 06 2006 17:23:05 LMT
;;
;;

(defvar ez-popup-menu (make-sparse-keymap "ez-popup"))
(defvar ez-popup-menu-contents-file-path "~/ez-popup-menu-contents")
;; (setq ez-popup-menu-contents-file-path "~/elisp/ez-popup-menu-contents")

(defvar ez-popup-menu-x-offset 100)
(defvar ez-popup-menu-y-offset 150)

;(define-key menu [kaigyo] '("new line" . newline))

(defmacro ez-popup-menu-update-contents (x)
  (list 'define-key 'menu '[ (list '1+ x)))


(defun ez-popup-menu-content-loaded-p ()
  "Return t when already loaded menu contents, otherwise return nil."
  (let ((menu ez-popup-menu))
    (listp (cadr menu))))

(defun ez-popup-menu-load-menu-contents ()
  "load menu contents file."
  (interactive)
  (let ((fn (expand-file-name ez-popup-menu-contents-file-path))
	(loaded nil)
	(menu (make-sparse-keymap "ez-popup")))
    (if (and (file-exists-p fn) (file-readable-p fn))
	(when (condition-case nil
		  (load fn t t t)
		(error "failed to load menu contents."))
	  (setq loaded t)
	  (message "load menu contents: OK"))
      (message (format "invalid menu contents file: %S" fn)))
    (when loaded
      (setq ez-popup-menu (copy-keymap menu))	;update current menu
    )))


(defun ez-popup-menu-key ()
  "popup ez-popup-menu by keydown"
  (interactive)
  (let ((menu ez-popup-menu)
	(ofs (list ez-popup-menu-x-offset ez-popup-menu-y-offset))
	(pos (cons (get-top-window) '())))
    (setq pos (cons ofs pos))
    (x-popup-menu pos menu)))


(defun get-top-window ()
  "Return the window which posisioned nearest frame's top/left."
  (let ((top-win (selected-window)))
    (save-selected-window
      (let ((wins '()))
	(walk-windows '(lambda (w)
			 (setq wins (cons w wins)))
		      t t)
	(let (w lf tp edge)
	  (while wins
	    (setq w (car wins))
	    (setq edge (window-edges w))
	    (setq tp (nth 1 edge))	; 上端
	    (setq lf (nth 0 edge))	; 左端

	    (cond ((< tp (nth 1 (window-edges top-win)))
		   (setq top-win w))
		  ((and (= tp (nth 1 (window-edges top-win)))
			(< lf (nth 0 (window-edges top-win))))
		   (setq top-win w)))
	    (setq wins (cdr wins))))))
    top-win))

;;
;; keybind
;;

; 右ボタン押下でメニュー開く
(define-key global-map [down-mouse-2] ez-popup-menu)

; M-1でメニューを開く
(global-set-key "\M-1" 'ez-popup-menu-key)

(provide 'ez-popup-menu)
