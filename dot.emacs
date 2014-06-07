;; .emacs  
(message "loading dot.emacs")

(setq load-path (append '("~/elisp") load-path))
(setq default-directory "~/")

;; package.el
;;
;;    M-x package-list-packages           インストール出来るパッケージ一覧を取得;;    M-x package-list-packages-no-fetch  インストール出来るパッケージ一覧を取得(更新なし)
;;    M-x package-install                 パッケージ名を指定してインストール
(when (eq system-type 'darwin)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq package-user-dir "~/elisp/elpa/")
  (package-initialize)
  ;; (require 'melpa)
)


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
