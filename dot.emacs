;; .emacs  
(message "loading dot.emacs")

(setq load-path (append '("~/Dropbox/elisp") load-path))
(setq default-directory "~/")

(load
 (cond ((featurep 'aquamacs) "dot.emacs-aquamacs")
       ((eq system-type 'gnu/linux) "dot.emacs-linux")
       (t "dot.emacs-mac")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((syntax . elisp))))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
