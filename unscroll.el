(defvar unscroll-point (make-marker)
  "Cursor position for next call to `unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to `unscroll'.")
(defvar unscroll-hscroll nil
  "Hscroll for next call to `unscroll'.")
(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))

(defadvice scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-down (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-left (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-right (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-one-line-ahead (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-one-line-behind (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-n-lines-ahead (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-n-lines-behind (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice beginning-of-buffer (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(defadvice end-of-buffer (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

;; for me
(defadvice sane-scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(put 'sane-scroll-up 'unscrollable t)
(defadvice sane-scroll-down (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))
(put 'sane-scroll-down 'unscrollable t)


;; (if running-under-xemacs
;;     (progn
;;       (defadvice scroll-up-command (before remember-for-unscroll activate compile)
;; 	"Remember where we started from, for `unscroll'."
;; 	(unscroll-maybe-remember))
;;       (defadvice scroll-down-command (before remember-for-unscroll activate compile)
;; 	"Remember where we started from, for `unscroll'."
;; 	(unscroll-maybe-remember))

;;       (put 'scroll-up-command 'unscrollable t)
;;       (put 'scroll-down-command 'unscrollable t)))

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-right 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-one-lines-ahead 'unscrollable t)
(put 'scroll-one-lines-behind 'unscrollable t)
(put 'scroll-n-lines-ahead 'unscrollable t)
(put 'scroll-n-lines-ahead 'unscrollable t)
(put 'beginning-of-buffer 'unscrollable t)
(put 'end-of-buffer 'unscrollable t)

(defun unscroll ()
  "Revert to `unscroll-point' and `unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))
