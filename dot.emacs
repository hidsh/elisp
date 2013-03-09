;; .emacs  
(message "loading dot.emacs")

(setq load-path (append '("~/Dropbox/elisp") load-path))


(load
 (if (featurep 'aquamacs)
     "dot.emacs-aquamacs"               ; for aquamacs
     "dot.emacs-gnu"))                  ; for gnu-emacs


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((syntax . elisp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
