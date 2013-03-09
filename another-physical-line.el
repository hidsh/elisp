;;物理行単位のカーソル移動(普通のWindowsエディタ風)
;;参考URL: http://www.sakura.ne.jp/~delmonta/emacs/16.txt
(defun next-phy-line ()
  "next physical line"
  (interactive)
  (if truncate-lines
      (next-line 1)
    (or (eq last-command 'vz-next-line)
        (setq vz-goal-column (% (current-column) (1- (window-width)))))
    (if (eolp)
        (progn
          (forward-char 1)
          (move-to-column vz-goal-column))
      (vertical-motion 1)
      (move-to-column (+ vz-goal-column (current-column))))))

(defun previous-phy-line ()
  "previosu physical line"
  (interactive)
  (if truncate-lines
      (previous-line 1)
    (or (eq last-command 'vz-next-line)
        (setq vz-goal-column (% (current-column) (1- (window-width)))))
    (let ((ncol (- vz-goal-column (window-width) -1)))
      (vertical-motion -1)
      (if (<= 0 ncol)
          (move-to-column ncol)
        (move-to-column (+ vz-goal-column (current-column))))))
  (setq this-command 'vz-next-line))    ;fake

(defun right-of-screen ()
  "physical end of line"
  (interactive)
  (vertical-motion 1)
  (if (not (eobp))
      (backward-char 1)))

(defun left-of-screen ()
  "physical start of line"
  (interactive)
  (vertical-motion 0))

(defvar vz-goal-column nil)

