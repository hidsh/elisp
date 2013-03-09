;;; find-file-existed-complementary.el --- interactive function 

;; Copyleft 2003 Hideaki Shishido

;; Author: Hideaki Shishido <hideaki_sh@ybb.ne.jp>
;; Maintainer: Hideaki Shishido <hideaki_sh@ybb.ne.jp>
;; Created: 31 July 2003
;; Version: 1.0
;; Keywords: file

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; WHAT IS THIS?
;;
;; This elisp is an interactive function. If you are editing one of
;; the complementary pair of files, you can find-file the other one
;; more easily using this additional function. 
;; By way of experiment...  
;; There are two files which are named "main.c" and "main.h". These
;; files have relation of complementary each other. and both file-name
;; has difference about extension only. Now you are editing main.c(one
;; of pair), and you want to find-file main.h (the other file). You
;; can do that only one step using this addtional function.
;;
;; HOW TO INSTALL
;; 
;; 1. In the first, add following lines in your .emacs.
;;   (require 'find-file-existed-complementary)
;;
;; 2. Setting keybinding for this function.
;;   .e.g
;;   (global-set-key "\C-xf" 'find-file-existed-complementary)
;;
;; 3. If you want to define complementary pair of files, you can add
;; following line.
;;   .e.g
;;   (setq find-file-existed-complementary-alist
;; 	(append '(("\\.cpp$" . "\\.h$"))
;; 		'(("\\.cpp$" . "\\.hpp$"))
;; 		find-file-existed-complementary-alist))
;; 
;; That's all.

;;; Change Log:
;;
;; Created: 31 July 2003

;;; Code:
(defvar find-file-existed-complementary-alist
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(("\\.html$" . "\\.php$")
     ("\\.c$" . "\\.h$")))
  "*Alist of complementary pair of extension patterns in filename.")


(defun extension-from-pattern (pattern)
  "Return true extension which is included period from search pattern."
  (let ((beg (string-match "\\." pattern))
	(end (string-match "\\$" pattern)))
    (substring pattern beg end)))


(defun return-complementary-file-name (file-name list)
  "Return complementary file name. or return nil in case of not found."
  (let ((new-name nil))
    (while list
      (let ((ext1 (car (car list)))
	    (ext2 (cdr (car list))))
	(if (not (null (string-match ext1 file-name)))
	    (progn
	      (setq new-name (concat
			      (file-name-sans-extension file-name)
			      (extension-from-pattern ext2)))
	      (if (file-exists-p new-name)
		  (setq list '(dummy))
		(setq new-name nil)))
	  (if (not (null (string-match ext2 file-name)))
	      (progn
		(setq new-name (concat
				(file-name-sans-extension file-name)
				(extension-from-pattern ext1)))
		(if (file-exists-p new-name)
		    (setq list '(dummy))
		  (setq new-name nil))))))
      (setq list (cdr list)))
    new-name))

(defun find-file-existed-complementary ()
  "Find existed complementary file of current buffer's file"
  (interactive)
  (if (not (null buffer-file-name))
      (let ((new-file-name (return-complementary-file-name
			    buffer-file-name
			    find-file-existed-complementary-alist)))
	(if (not (null new-file-name))
	    (find-file new-file-name)
	  (message "\"%s\" does not have a complementary file."
		   (file-name-nondirectory buffer-file-name))))
    (message "This buffer does not have a real file.")))

;;; find-file-existed-complementary ends here

