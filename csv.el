;;; csv.el --- commands to process CSV records in an Emacs buffer

;; Copyright (C) 2003 Francis J. Wright

;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; Time-stamp: <22 November 2003>
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/
;; Version: $Id: csv.el,v 1.4 2003/11/22 12:09:12 fjw Exp $
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is intended for use with GNU Emacs 21, and implements
;; the following commands to process CSV (comma-separated value)
;; records in the region in an Emacs buffer:
;;
;; `csv-sort-fields' and `csv-sort-numeric-fields' sort respectively
;; lexicographically and numerically on a specified field.  They are
;; based closely on, and use, code in `sort.el' and the user
;; interfaces are identical to those for the standard commands
;; `sort-fields' and `sort-numeric-fields'.
;;
;; `csv-kill-fields' and `csv-yank-fields' respectively kill and yank
;; fields (or columns), although they do not use the normal kill ring.

;; Note: In general CSV data, fields can be delimited by double-quote
;; characters (and must if they contain commas).  This implementation
;; supports quoted fields provided the quotes are double-quotes that
;; are IMMEDIATELY preceded or followed by comma or newline.

;;; Installation:

;; Put this file somewhere that Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it (recommended), and put this in your .emacs:
;;
;; (autoload 'csv-sort-fields "csv"
;;   "Sort CSV data lexicographically by field." t)
;; (autoload 'csv-sort-numeric-fields "csv"
;;   "Sort CSV data numerically by field." t)
;; (autoload 'csv-kill-fields "csv" "Kill CSV data by field." t)
;; (autoload 'csv-yank-fields "csv" "Yank CSV data by field." t)

;;; History:

;; This first was begun on 15 November 2003 to provide lexicographic
;; sorting of simple CSV data by field.

;;; To do (maybe):

;; Align columns by padding with white space.
;; Convert comma-separated to space- or tab-separated.

;;; Code:

(require 'sort)

(defun csv-sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by commas and numbered from 1 up.
Null fields are allowed anywhere and sort before any non-null field.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "*p\nr")
  (sort-fields-1
   field beg end
   ;; STARTKEYFUN moves from the start of the record to the start of
   ;; the key.  It may return either a non-nil value to be used as the
   ;; key, or else the key is the substring between the values of
   ;; point after STARTKEYFUN and ENDKEYFUN are called.  If
   ;; STARTKEYFUN is nil, the key starts at the beginning of the
   ;; record.
   (lambda () (csv-sort-skip-fields field) nil)
   ;; ENDKEYFUN moves from the start of the sort key to the end of the
   ;; sort key.  ENDKEYFUN may be nil if STARTKEYFUN returns a value
   ;; or if it would be the same as ENDRECFUN.
   (lambda () (skip-chars-forward "^,\n"))
   ))

(defun csv-sort-numeric-fields (field beg end)
  "Sort lines in region numerically by the ARGth field of each line.
Fields are separated by commas and numbered from 1 up.
Null fields are allowed anywhere and sort as zeros.
Specified field must contain a number in each line of the region,
which may begin with \"0x\" or \"0\" for hexadecimal and octal values.
Otherwise, the number is interpreted according to sort-numeric-base.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort."
  (interactive "*p\nr")
  (sort-fields-1 field beg end
		 (lambda ()
		   (csv-sort-skip-fields field)
		   (let* ((case-fold-search t)
			  (base
			   (if (looking-at "\\(0x\\)[0-9a-f]\\|\\(0\\)[0-7]")
			       (cond ((match-beginning 1)
				      (goto-char (match-end 1))
				      16)
				     ((match-beginning 2)
				      (goto-char (match-end 2))
				      8)
				     (t nil)))))
		     (string-to-number (buffer-substring (point)
							 (save-excursion
							   (forward-sexp 1)
							   (point)))
				       (or base sort-numeric-base))))
		 nil))

(defsubst csv-forward-field ()
  "Skip forward over one field."
  (if (eq (following-char) ?\")
      (forward-sexp)
    (skip-chars-forward "^,\n")))

(defsubst csv-backward-field ()
  "Skip backward over one field."
  (if (eq (preceding-char) ?\")
      (backward-sexp)
    (skip-chars-backward "^,\n")))

(defun csv-sort-skip-fields (n &optional yank)
  "Position point at the beginning of field N on the current line.
Fields are separated by commas\; null terminal field allowed.
Assumes point is initially at the beginning of the line.
YANK non-nil allows N to be 1 greater than the number of fields."
  (if (> n 0)
      ;; Skip across N - 1 fields.
      (let ((i (1- n)))
	(while (> i 0)
	  (csv-forward-field)
	  (if (eolp)
	      (or (and yank (= i 1))
		  (error "Line has too few fields: %s"
			 (buffer-substring
			  (save-excursion (beginning-of-line) (point))
			  (save-excursion (end-of-line) (point)))))
	    (forward-char))		; skip comma
	  (setq i (1- i))))
    (end-of-line)
    ;; Skip back across -N - 1 fields.
    (let ((i (1- (- n))))
      (while (> i 0)
	(csv-backward-field)
	(if (bolp)
	    (error "Line has too few fields: %s"
		   (buffer-substring
		    (save-excursion (beginning-of-line) (point))
		    (save-excursion (end-of-line) (point)))))
	(backward-char)			; skip comma
	(setq i (1- i)))
      ;; Position at the front of the field
      ;; even if moving backwards.
      (csv-backward-field))))

(defvar csv-killed-fields nil
  "The last column of fields killed by `csv-kill-fields'.")

(defun csv-kill-fields (field beg end)
  "Kill the ARGth field of each line in the region.
The fields are stored for use by `csv-yank-fields'.
Fields are separated by commas and numbered from 1 up.
Null fields are allowed anywhere.
With a negative arg, kills the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to process."
  (interactive "*p\nr")
  (if (zerop field) (setq field 1))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (setq csv-killed-fields nil)
      (while (not (eobp))
	;; Move to start of field to kill:
	(csv-sort-skip-fields field)
	;; Kill to end of field (cf. `kill-region'):
	(setq csv-killed-fields
	      (cons
	       (delete-and-extract-region
		(point)
		(progn (csv-forward-field) (point)))
	       csv-killed-fields))
	(if (eolp) (delete-char -1)	; delete trailing comma at eol
	  (delete-char 1))		; or following comma otherwise
	(forward-line))))
  (setq csv-killed-fields (nreverse csv-killed-fields)))

(defun csv-yank-fields (field beg end)
  "Yank fields as the ARGth field of each line in the region.
The fields yanked are those last killed by `csv-kill-fields'.
Fields are separated by commas and numbered from 1 up.
Null fields are allowed anywhere.
With a negative arg, yanks into the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to process."
  (interactive "*p\nr")
  (cond ((zerop field) (setq field 1))
	((< field 0) (setq field (1+ field))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((fields csv-killed-fields))
	(while (not (eobp))
	  ;; Yank at start of specified field if possible, otherwise
	  ;; yank at end of record:
	  (if (zerop field)
	      (end-of-line)
	    (csv-sort-skip-fields field 'yank))
	  (and (eolp) (insert ?,))
	  (when fields
	    (insert (car fields))
	    (setq fields (cdr fields)))
	  (or (eolp) (insert ?,))
	  (forward-line))))))

(provide 'csv)

;;; csv.el ends here
