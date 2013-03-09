;; my-sendkeys.el
;; THX to http://www.clemburg.com/install/my-sendkeys.el
;;

(setq my-sendkeys-tempfile "c:\\work\\tmp\\my-sendkeys-tempfile.txt")

(defun my-empty-string-p (s)
  (or (null s) (string-equal s "")))

(defun my-sendkeys (key-string target &optional app-path switch-back)
  "Send keys described in KEY-STRING to window with title TARGET or application located at APP-PATH. After sending, switch back to current buffer if SWITCH-BACK is not empty."
  (interactive "sSend Keys: \nsTo Window Titled: \nsApplication Path: \nsSwitch Back to Emacs: ")
  (with-temp-file my-sendkeys-tempfile
    (insert key-string))
  (shell-command (concat "sendkeys" 
			 " "
			 "\"" my-sendkeys-tempfile "\"" 
			 " "
			 "\"" target "\""
			 " "
			 "\"" app-path "\""))
  (unless (my-empty-string-p switch-back)
    (focus-frame (window-frame))))

(defun my-copy-region-to-window (title &optional replace)
  "Copy contents of region to window named by TITLE, replacing contents there by selecting everything and pasting if REPLACE is not empty."
  (interactive "sWindow title: \nsReplace existing content: ")
  (copy-region-as-kill (region-beginning) (region-end))
  (if (my-empty-string-p replace)
      (my-sendkeys "^v" title)
    (my-sendkeys "^a^v" title)))

(defun my-copy-buffer-to-window (title &optional replace)
  "Copy contents of buffer to window named by TITLE, replacing contents there by selecting everything and pasting if REPLACE is not empty."
  (interactive "sWindow title: \nsReplace existing content: ")
  (save-excursion
    (mark-whole-buffer)
    (my-copy-region-to-window title replace)))



