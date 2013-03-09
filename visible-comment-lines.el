;; visible-comment-lines.el --- minor mode, show/hide comment lines
;;
;;	$Id: visible-comment-lines.el,v 1.6 2003/10/18 09:52:57 gnrr Exp gnrr $
;;
;; Filename: visible-comment-lines.el
;; Last modified: Sat Oct 18 2003 18:52:57 JST
;;
;;
;; visible-comment-lines.el comes with ABSOLUTELY NO WARRANTY.
;; This software is distributed under the GNU General Public License.
;;
;; What is this:
;; Visible-comment-lines-mode is minor-mode. This elisp provides show/hide
;; control about comment lines. All comment line is hidden when
;; entered visible-comment-lines-mode. And to exit from this minor-mode,
;; It's show them all.
;;
;; Install:
;;  1. move "visible-comment-lines.el" into the directory as your load-path.
;;  2. add the following line to your dot.emacs.
;;     (require 'visible-comment-lines)
;;
;; Usage:
;;  1. invoke visible-comment-lines-mode minor-mode by C-x n ;
;;  2. exit from this minor-mode, punch again C-x n ;
;;     If you don't like this keybind, you can change it by additonal
;;     line into your dot.emacs as you like such below.
;;     (global-set-key "\C-x\C-i" 'visible-comment-lines-mode)
;;
;; Todo:
;;  1. define local-keymap for moving cursor


;; ;; my setting
;; (require 'discrete)

;;
;; internal variable
;;

;; stored keymap before change to this-mode's one.
(defvar visible-comment-lines-mode-previous-keymap nil)
;; stored overlay in list
(defvar visible-comment-lines-mode-overlays ())

;;
;; sub routines
;;

;; show
(defun show-all-comment-lines ()
  "カレントバッファ上のコメント行をすべて表示する。"
  (interactive)
  (mapcar (function (lambda (olay)
		      (overlay-put olay 'invisible nil)))
	  visible-comment-lines-mode-overlays)
  (setq visible-comment-lines-mode-overlays ()))

;; hide
(defun hide-all-comment-lines ()
  "カレントバッファ上のコメント行をすべて隠す。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while (< (point) (point-max))
      (let (start end olay)
	(if (whole-comment-line-p)
	    (progn
	      (setq start (progn
			    (beginning-of-line)
			    (point)))
	      (setq end (progn
			  (forward-line)
			  (point)))
	      (when (< start end)
		(setq olay (make-overlay start end))
		(overlay-put olay 'invisible t)
		(setq visible-comment-lines-mode-overlays
		      (cons olay visible-comment-lines-mode-overlays))))
	  (forward-line))))))

;;
;; rules of minor mode
;;

;; enter the minor mode
(defun visible-comment-lines-mode-enter ()
  (setq visible-comment-lines-mode-previous-keymap
	(copy-keymap (current-local-map)))
  (use-local-map visible-comment-lines-mode-map)
  (hide-all-comment-lines))

;; exit from minor mode
(defun visible-comment-lines-mode-exit ()
  (use-local-map visible-comment-lines-mode-previous-keymap)
  (show-all-comment-lines))

;; mode variable
(defvar visible-comment-lines-mode nil
  "Mode variable for visible-comment-lines-mode minor mode.")
(make-variable-buffer-local 'visible-comment-lines-mode)

;; keymap
(defvar visible-comment-lines-mode-map nil
  "Keymap for visible-comment-lines-mode minor mode.")

(if visible-comment-lines-mode-map
    nil
  (setq visible-comment-lines-mode-map (make-sparse-keymap))
  (define-key visible-comment-lines-mode-map "\C-f" 'forward-char)
  (define-key visible-comment-lines-mode-map "\C-b" 'backward-char)
  (define-key visible-comment-lines-mode-map "\C-n" 'next-line)
  (define-key visible-comment-lines-mode-map "\C-p" 'previous-line)
)

;; mode line
(if (not (assq 'visible-comment-lines-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(visible-comment-lines-mode " Hide-Comment") minor-mode-alist)))

;; entrance
(defun visible-comment-lines-mode (&optional arg)
  "Minor mode, in order to show/hide all comment lines."
  (interactive "P")
  (setq visible-comment-lines-mode
	(if (null arg)
	    (not visible-comment-lines-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if visible-comment-lines-mode
      ;; hide
      (visible-comment-lines-mode-enter)
    ;; show
    (visible-comment-lines-mode-exit))
  (force-mode-line-update))

;; global keybind
(global-set-key "\C-xn;" 'visible-comment-lines-mode)

;;;
;;; end
;;;
(provide 'visible-comment-lines)

;;; visible-comment-lines.el ends here
