;; Filename: sx.el
;; Last modified: Œ—j“ú 7Œ 10 2006 12:17:16 “Œ‹ (•W€)

(defun sx-remove-object-file ()
  "remove object file"
  (interactive)
  (let ((ext ".o")
	(cf (buffer-file-name))
	of)
    (when cf
      (setq of (concat (file-name-sans-extension (expand-file-name cf))
		       ext))
      (when (file-exists-p of)
	(delete-file of)))))

(add-hook 'after-save-hook
	  (lambda ()
	    (let ((cf (buffer-file-name)))
	    (when (and cf (string= (file-name-extension cf) "h"))
	      (sx-remove-object-file)))))

