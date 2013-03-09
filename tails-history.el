;; http://www.ganaware.jp/archives/2004/04/emacs_minibuffe.html
;;
(define-key minibuffer-local-map [?\M-p] 'gana-previous-history-element)
(define-key minibuffer-local-completion-map [?\M-p] 'gana-previous-history-element)
(define-key minibuffer-local-must-match-map [?\M-p] 'gana-previous-history-element)
(define-key minibuffer-local-ns-map [?\M-p] 'gana-previous-history-element)

(define-key minibuffer-local-map [?\M-n] 'gana-next-history-element)
(define-key minibuffer-local-completion-map [?\M-n] 'gana-next-history-element)
(define-key minibuffer-local-must-match-map [?\M-n] 'gana-next-history-element)
(define-key minibuffer-local-ns-map [?\M-n] 'gana-next-history-element)

(defun gana-previous-history-element (n)
 "Inserts the previous element of the minibuffer history into the minibuffer."
 (interactive "p")
 (gana-next-history-element (- n)))

(defun gana-next-history-element (n)
 "Insert the next element of the minibuffer history into the minibuffer."
 (interactive "p")
 (setq n (if (< n 0) -1 1))
 (or (zerop n)
   (let* ((narg (- minibuffer-history-position n))
       (minimum (if minibuffer-default -1 0))
       elt
       minibuffer-returned-to-present
       (prefix (buffer-substring (field-beginning (point-max)) (point)))
       (prefix-len (length prefix))
       (m-history (symbol-value minibuffer-history-variable)))

    (if (and (zerop minibuffer-history-position)
         (null minibuffer-text-before-history))
      (setq minibuffer-text-before-history (field-string (point-max))))

    (while (and (<= 1 narg)
          (<= narg (length m-history))
          (let* ((elt (nth (1- narg) m-history))
              (elt-len (length elt)))
           (or (< elt-len prefix-len)
             (not (string= prefix (substring elt 0 prefix-len)))))
          )
     (setq narg (- narg n)))
    (if (<= narg 0)
      (error "End of history; no next item"))
    
    (if (< narg minimum)
      (if minibuffer-default
        (error "End of history; no next item")
       (error "End of history; no default available")))
    (if (> narg (length (symbol-value minibuffer-history-variable)))
      (error "Beginning of history; no preceding item"))
    (unless (or (eq last-command 'gana-next-history-element)
          (eq last-command 'gana-previous-history-element))
     (let ((prompt-end (field-beginning (point-max))))
      (set (make-local-variable 'minibuffer-temporary-goal-position)
         (cond ((<= (point) prompt-end) prompt-end)
                    ;((eobp) nil)
            (t (point))))))

    (goto-char (point-max))
    (delete-field)
    (setq minibuffer-history-position narg)
    (cond ((= narg -1)
        (setq elt minibuffer-default))
       ((= narg 0)
        (setq elt (or minibuffer-text-before-history ""))
        (setq minibuffer-returned-to-present t)
        (setq minibuffer-text-before-history nil))
       (t (setq elt (nth (1- minibuffer-history-position)
                (symbol-value minibuffer-history-variable)))))
    (insert
     (if (and (eq minibuffer-history-sexp-flag (minibuffer-depth))
         (not minibuffer-returned-to-present))
       (let ((print-level nil))
        (prin1-to-string elt))
      elt))
    (goto-char (or minibuffer-temporary-goal-position (point-max))))))
