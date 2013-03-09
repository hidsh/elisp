;;; bm.el  -- Visible bookmarks in buffer.

;; Copyrigth (C) 2000, 2001, 2002  Jo Odland

;; Author: Jo Odland <jood@online.no>
;; Time-stamp:	<Fri Dec  6 10:58:07 2002  jood>
;; Version: $Id: bm.el,v 1.13 2002/12/06 09:58:43 jood Exp $
;; Keywords; bookmark
;; URL: http://home.online.no/~jood/emacs/bm.el

;; Portions Copyright (C) 2002 by Ben Key
;; Updated by Ben Key <bkey1@tampabay.rr.com> on 2002-12-05
;; to add support for XEmacs


;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Description:
;;
;;   This package was created because I missed the bookmarks from M$
;;   Visual Studio. They provide an easy way to navigate in a buffer.
;;
;;   bm.el provides visible, buffer local, bookmarks and the ability
;;   to jump forward and backward to the next bookmark.
;;
;;   The use of overlays for bookmarks was inspired by highline.el by
;;   Vinicius Jose Latorre <vinicius@cpqd.com.br>.
;;
;;   This package is developed and testet on GNU Emacs 21.2. It should
;;   also work on GNU Emacs 20.x and XEmacs 21.x


;; Installation:
;;
;;   To use bm.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'bm)
;;
;; or
;;
;;   (autoload 'bm-toggle	 "bm" "Toggle bookmark in current buffer." t)
;;   (autoload 'bm-goto-next	 "bm" "Goto bookmark."			   t)
;;   (autoload 'bm-goto-previous "bm" "Goto previous bookmark."		   t)


;; Configuration:
;;
;;   To make it easier to use, assign the commands to some keys.
;;
;;   M$ Visual Studio key setup.
;;     (global-set-key (kbd "<C-f2>") 'bm-toggle)
;;     (global-set-key (kbd "<f2>")   'bm-goto-next)
;;     (global-set-key (kbd "<S-f2>") 'bm-goto-previous)
;;


;; Acknowledgements:
;;
;; Thanks to Ben Key <bkey1@tampabay.rr.com> for XEmacs support.
;;


;; Todo:
;;
;;  - stop at end of buffer before wrapping (like isearch-forward).
;;

;; Code:
;;

;; xemacs needs overlay emulation package
(eval-and-compile
  (unless (fboundp 'overlay-lists)
    (require 'overlay)))


(defconst bm-version "$Id: bm.el,v 1.13 2002/12/06 09:58:43 jood Exp $"
  "RCS version of bm.el")


(defgroup bm nil
  "Toggle visible, buffer local, bookmarks."
  :link '(emacs-library-link :tag "Source Lisp File" "bm.el")
  :group 'faces
  :group 'editing
  :prefix "bm-")


(defcustom bm-face 'bm-face
  "*Specify face used to highlight the current line."
  :type 'face
  :group 'bm)


(defcustom bm-priority 0
  "*Specify bm overlay priority.

Higher integer means higher priority, so bm overlay will have precedence
over overlays with lower priority.  *Don't* use negative number."
  :type 'integer
  :group 'bm)


(defface bm-face
  '((((class grayscale) (background light)) (:background "DimGray"))
    (((class grayscale) (background dark))  (:background "LightGray"))
    (((class color) (background light)) (:foreground "White" :background "goldenrod"))
    (((class color) (background dark))	(:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line."
  :group 'bm)



(defcustom bm-wrap t
 "*Specify if bookmark search should wrap.

nil, don't wrap when there are no more bookmarks.
t, wrap."
 :type 'boolean
 :group 'bm)


(defcustom bm-recenter nil
  "*Specify if the buffer should be recentered around the bookmark
after a `bm-goto-next' or a `bm-goto-previous'."
  :type 'boolean
  :group 'bm)


(defvar bm-regexp-history nil
  "Bookmark regexp history.")


(defun bm-customize nil
  "Customize bm group"
  (interactive)
  (customize-group 'bm))


(defun bm-bookmark-add nil
  "Add bookmark at current line. Do nothing if no bookmark is
present."
  (if (not (bm-bookmark-at (point)))
      (let ((bookmark (make-overlay (bm-start-position)
				    (bm-end-position))))
	(overlay-put bookmark 'face bm-face)
	(overlay-put bookmark 'evaporate t)
	(unless (featurep 'xemacs)
	  (overlay-put bookmark 'priority bm-priority)
	  (overlay-put bookmark 'modification-hooks '(bm-freeze))
	  (overlay-put bookmark 'insert-in-front-hooks '(bm-freeze-in-front))
	  (overlay-put bookmark 'insert-behind-hooks '(bm-freeze)))
	(overlay-put bookmark 'category "bm")
	bookmark)))


(defun bm-bookmark-remove (&optional bookmark)
  "Remove bookmark at point."
  (if (not bookmark)
      (setq bookmark (bm-bookmark-at (point))))

  (if (bm-bookmarkp bookmark)
      (delete-overlay bookmark)))




;;;###autoload
(defun bm-toggle nil
  "Toggle bookmark in current buffer."
  (interactive)
  (let ((bookmark (bm-bookmark-at (point))))
    (if bookmark
	(bm-bookmark-remove bookmark)
      (bm-bookmark-add))))


(defun bm-count nil
  "Display the number of bookmarks in current buffer."
  (interactive)
  (message "Buffer contains %d bookmark(s)." (length (bm-bookmarks))))


(defun bm-start-position nil
  "Return the bookmark start position."
  (point-at-bol))


(defun bm-end-position nil
  "Return the bookmark end position."
  (min (point-max) (+ 1 (point-at-eol))))


(defun bm-freeze-in-front (overlay after begin end &optional len)
  "Prevent overlay from being extenden to multiple lines. When
inserting in front of overlay move overlay forward."
  (if after
      (move-overlay overlay (bm-start-position) (bm-end-position))))


(defun bm-freeze (overlay after begin end &optional len)
  "Prevent overlay from being extended to multiple lines. When
inserting inside or behind the overlay, keep the original start
postion."
  (if after
      (let ((bm-start (overlay-start overlay)))
	(if bm-start
	    (move-overlay overlay
			  bm-start
			  (save-excursion
			    (goto-char bm-start)
			    (bm-end-position)))))))


(defun bm-lessp (first second)
  "Compare two bookmarks or points. Return t if first is less than second."
  (if (bm-bookmarkp first)
      (setq first (overlay-start first)))
  (if (bm-bookmarkp second)
      (setq second (overlay-start second)))
  (< first second))


(defun bm-bookmarkp (bookmark)
  "Return t if bookmark is a bookmark."
  (if (and (overlayp bookmark)
	   (string= (overlay-get bookmark 'category) "bm"))
      bookmark
    nil))


(defun bm-bookmark-at (point)
  "Get bookmark at point."
  (let ((overlays (overlays-at point))
	(bookmark nil))
    (while (and (not bookmark) overlays)
      (if (bm-bookmarkp (car overlays))
	  (setq bookmark (car overlays))
	(setq overlays (cdr overlays))))
    bookmark))


(defun bm-bookmarks (&optional all)
  "Return all visible bookmarks in buffer. If optional parameter all is given
return all bookmarks in buffer."
  ;; get all overlays
  (let ((overlays (append (car (overlay-lists)) (cdr (overlay-lists))))
	(overlay nil)
	(bookmarks nil))
    (while overlays
      (setq overlay (car overlays))
      ;; get all visible bookmarks
      (if (bm-bookmarkp overlay)
	  (if (or all (and (>= (overlay-start overlay) (point-min))
			   (<= (overlay-end overlay) (point-max))))
	      (setq bookmarks (cons overlay bookmarks))))
      (setq overlays (cdr overlays)))

    (sort bookmarks 'bm-lessp)))


;;;###autoload
(defun bm-goto-next nil
  "Goto bookmark."
  (interactive)
  (let ((found nil)
	(bookmark nil)
	(bookmarks (bm-bookmarks)))
    (if bookmarks
	(progn
	  (while (and (not found) bookmarks)
	    (setq bookmark (car bookmarks))
	    (if (<= (overlay-start bookmark) (point))
		(setq bookmarks (cdr bookmarks))
	      (setq found t)))

	  ;; jump
	  (if found
	      (bm-goto bookmark)
	    (if (not bm-wrap)
		(message "No next bookmark.")
	      (bm-goto (car (bm-bookmarks)))
	      (message "No next bookmark... wrapping."))))
      (message "No bookmarks defined."))))


;;;###autoload
(defun bm-goto-previous nil
  "Goto previous bookmark."
  (interactive)
  (let ((found nil)
	(bookmark nil)
	(bookmarks (reverse (bm-bookmarks))))
    (if bookmarks
	(progn
	  (while (and (not found) bookmarks)
	    (setq bookmark (car bookmarks))
	    (if (>= (overlay-start bookmark) (point))
		(setq bookmarks (cdr bookmarks))
	      (setq found t)))

	  ;; jump
	  (if found
	      (bm-goto bookmark)
	    (if (not bm-wrap)
		(message "No previous bookmark.")
	      (bm-goto (car (reverse (bm-bookmarks))))
	      (message "No previous bookmark... wrapping."))))
      (message "No bookmarks defined."))))


(defun bm-remove-all (&optional all)
  "Delete all visible bookmarks in current buffer. If optional
parameter all is given delete all bookmarks in buffer."
  (interactive "P")
  (mapcar 'bm-bookmark-remove (bm-bookmarks all)))


(defun bm-toggle-wrapping nil
  "Toggle wrapping on/off, when searching for next bookmark."
  (interactive)
  (setq bm-wrap (not bm-wrap))
  (if bm-wrap
      (message "Wrapping on.")
    (message "Wrapping off.")))


(defun bm-goto (bookmark)
  "Goto selected bookmark."
  (if (bm-bookmarkp bookmark)
      (progn
	(goto-char (overlay-start bookmark))
	(if bm-recenter
	    (recenter)))
    (message "Bookmark not found.")))


(defun bm-bookmark-regexp nil
  "Set bookmark on lines that matches regexp."
  (interactive)
  (bm-bookmark-regexp-region  (point-min) (point-max)))


(defun bm-bookmark-regexp-region (beg end)
  "Set bookmark on lines that matches regexp in region."
  (interactive "r")
  (let ((regexp (read-from-minibuffer "regexp: " nil nil nil 'bm-regexp-history)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
	(bm-bookmark-add)
	(forward-line 1)))))


;; bm.el ends here
(provide 'bm)
