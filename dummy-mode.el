(defcustom original-mode-hook nil
"Normal hook run when entering original mode."
:type 'hook
:options '(turn-on-auto-fill turn-on-flyspell)
:group 'data)

(defvar original-font-lock-keywords
`(("^//.*" 0 font-lock-comment-face t)
"default font-lock-keywords")
)

(defvar original-mode-syntax-table nil
"syntax table used in original mode")
(setq original-mode-syntax-table (make-syntax-table))

(defvar original-mode-map
(let ((map (make-sparse-keymap)))
(define-key map '[(control c) (\;)] 'comment-region)
map))

(defun original-mode ()
(interactive)
(kill-all-local-variables)
(use-local-map original-mode-map)

(make-local-variable 'comment-start)
(setq comment-start "//")
; (make-local-variable 'comment-end)
; (setq comment-end "\n")
(make-local-variable 'parse-sexp-ignore-comments)
(setq parse-sexp-ignore-comments t)

(make-local-variable 'font-lock-defaults)
(setq mode-name "original"
major-mode 'original-mode)
(setq font-lock-defaults `(original-font-lock-keywords nil))
(set-syntax-table original-mode-syntax-table)
(run-hooks 'original-mode-hook)
)
(provide 'original-mode)
