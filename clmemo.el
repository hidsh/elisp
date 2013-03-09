;;; clmemo.el --- Change Log MEMO -*-emacs-lisp-*-

;; Copyright (c) 2002, 2003 Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; $Id: clmemo.el,v 1.46 2003/07/28 04:58:56 m1378502 Rel $

;; Author: Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; clmemo is minor mode for editing ChangeLog MEMO.

;; ChangeLog memo is a kind of a diary and a memo file.  For more
;; information, see web page <http://namazu.org/~satoru/unimag/1/>.
;; `M-x clmemo' open ChangeLog memo file directly in ChangeLog MEMO
;; mode.  Select your favorite entry with completion.  For a complition
;; list, use `clmemo-entry-list'.

;; The latest clmemo.el is available at:
;;
;;   http://isweb22.infoseek.co.jp/computer/pop-club/emacs/clmemo.el

;; Special thanks to rubikitch for clmemo-yank, clmemo-indent-region,
;; and bug fix of quitting entry.  Great thanks to Tetsuya Irie, Souhei
;; Kawazu, Katsuwo Mogi, Hideaki Shirai, and ELF ML members for all
;; their help.

;;; How to install:

;; To install, put this in your .emacs file:
;;
;;   (autoload 'clmemo "clmemo" "ChangeLog memo mode." t)

;; And set your favorite key-binding and entries of MEMO.  For example:
;;
;;   (define-key ctl-x-map "M" 'clmemo)
;;   (setq clmemo-entry-list
;;        '("Emacs" "StarWars" '("lotr" . "The Load of the Rings") etc...))

;; Put this in the bottom of your ChangeLog MEMO file.
;;
;;   ^L
;;   ;;; Local Variables: ***
;;   ;;; mode: change-log ***
;;   ;;; clmemo-mode: t ***
;;   ;;; End: ***
;;
;; This code tells Emacs to set major mode change-log and toggle minor
;; mode clmemo-mode ON in your ChangeLog MEMO.  For more information,
;; see section "File Variables" in `GNU Emacs Reference Manual'.
;;
;; `^L' is a page delimiter.  You can insert it with C-q C-l.
;;
;; If you are Japanese, it is good idea to specify file coding system
;; like this;
;;
;;   ^L
;;   ;;; Local Variables: ***
;;   ;;; mode: change-log ***
;;   ;;; clmemo-mode: t ***
;;   ;;; coding: iso-2022-jp ***
;;   ;;; End: ***
;;

;;; ChangeLog:

;; $Log: clmemo.el,v $
;; Revision 1.46  2003/07/28 04:58:56  m1378502
;; Require add-log.el because clmemo prevent add-log from evaluating add-log-time-format.
;;
;; Revision 1.45  2003/07/17 20:41:19  m1378502
;; (clmemo-schedule-header): New user option.
;; (clmemo-schedule): New function.
;;
;; Revision 1.44  2003/07/17 20:11:20  m1378502
;; (clmemo): Fix bug about add-log-time-format.
;;
;; Revision 1.43  2003/07/15 03:01:35  m1378502
;; Fix typo.
;;
;; Revision 1.42  2003/07/14 21:45:53  m1378502
;; (clmemo-yank): Do not insert extra TAB at first line of yank text.
;;
;; Revision 1.41  2003/07/14 18:06:18  m1378502
;; (clmemo-indent-only-one-tab): New user option.
;; (clmemo-indent-region): Use it.
;;
;; Revision 1.40  2003/07/14 17:49:04  m1378502
;; (clmemo-time-string-with-weekday): New user option.
;; (clmemo): Use it.
;; (add-log-iso8601-time-string-with-weekday): New function.
;; Contributed by Satoru Takabayashi <satoru@namazu.org>.
;; (clmemo-format-header-with-weekday)
;; (clmemo-format-header-without-weekday): New function.
;;
;; Revision 1.39  2003/05/19 16:45:10  m1378502
;; Change URL and Doc fix.
;;
;; Revision 1.38  2003/05/19 11:46:53  m1378502
;; Fixed previous change.
;;
;; Revision 1.37  2003/05/12 17:40:04  m1378502
;; Fixed bug that reading subentry prevents entry from expanding with cdr of
;; clmemo-entry-list.
;; Reported by Souhei Kawazu <wham@free.japandesign.ne.jp>
;; (clmemo-subentry-p): New function.
;; (clmemo-get-entry, clmemo-completing-read): Use it.
;;
;; Revision 1.36  2002/12/16 22:06:42  m1378502
;; Version 0.6.0 -- Work with clkwd.el
;; Move provide code head of file.
;; Load clkwd.el if found.
;; (clmemo-www-item-insert): Move to clkwd.el
;; (clmemo-mode): Doc fix.  show key-map correctly.
;;
;; Revision 1.35  2002/10/16 09:11:08  m1378502
;; Doc fix.
;;
;; Revision 1.34  2002/10/14 14:04:02  m1378502
;; Fixed previous bug.
;; (clmemo-mode): Toggle off clmemo-mode only when get negative arg.
;;
;; Revision 1.33  2002/10/14 08:23:20  Mark
;; Fixed problem that failed to set clmemo-mode-map.
;;
;; Revision 1.32  2002/10/13 15:10:26  m1378502
;; (clmemo): With prefix arg (C-u), if already visited the ChangeLog memo file,
;; ask entry and insert it in the date where point is.
;;
;; Revision 1.31  2002/10/13 14:09:58  m1378502
;; clmemo-mode as minor mode.
;; Doc fix.
;; (clmemo-mode): New var.
;; (clmemo-mode): New func.
;;
;; Revision 1.30  2002/10/02 11:39:02  m1378502
;; Toggle open other window when called with C-u 0 and C-u 1.
;;
;; Revision 1.29  2002/09/28 03:40:00  m1378502
;; Rename variable from clmemo-entry-alist to clmemo-entry-list.
;;
;; Revision 1.28  2002/09/27 20:27:14  m1378502
;; (clmemo-completing-read): Accept not only alist but also list
;; even in Emacs 21 or less.
;; Reported by Katsuwo Mogi <mogi_k@hotmail.com>
;; Thanks for information and suggestion to:
;;  Hideaki SHIRAI <shirai@rdmg.mgcs.mei.co.jp> and
;;  IRIE Tetsuya <irie@t.email.ne.jp>.
;;
;; Revision 1.27  2002/09/17 14:57:22  m1378502
;; Fixed typo.
;;
;; Revision 1.26  2002/09/17 14:21:46  m1378502
;; Fixed previous change.
;;
;; Revision 1.25  2002/09/12 12:56:11  m1378502
;; Bind key:
;;  C-c C-n, C-c C-p: Next (Previous) title.
;;  C-c C-f, C-c C-b: Next (Previous) entry.
;;
;; Revision 1.24  2002/09/11 17:21:46  m1378502
;; Fixed another bug.
;; (clmemo): Set clmemo-mode if it was, Otherwise do not set clmemo-mode.
;;
;; Revision 1.23  2002/09/11 17:07:48  m1378502
;; (clmemo): Set clmemo-mode if it was, Otherwise do not set clmemo-mode.
;;
;; Revision 1.22  2002/09/11 12:26:02  m1378502
;; Doc fix.
;;
;; Revision 1.21  2002/09/11 12:22:31  m1378502
;; New mode: ChangeLog MEMO mode.
;; Some doc fix.
;;
;; Revision 1.20  2002/09/11 11:37:24  m1378502
;; Support yank and indent specialized for CLMEMO.
;; (clmemo-yank, clmemo-indent-region): New function.
;; Patch by rubikitch <rubikitch@ruby-lang.org>
;;
;; Revision 1.19  2002/09/09 02:14:46  m1378502
;; Fixed bug that `C-u C-u C-x M' output results in reverse order.
;; (clmemo): C-u C-u C-u then output is in reverse order.
;;
;; Revision 1.18  2002/09/07 09:13:03  m1378502
;; (clmemo): Prefix argument twice (C-u C-u), then call `clgrep-clmemo'.
;;
;; Revision 1.17  2002/09/05 05:56:04  m1378502
;; (clmemo-subentry-punctuation-char): New variable.
;; Customize punctuation char of subentry.
;; defualt is `(SUBENTRY)'.
;;
;; Revision 1.16  2002/09/05 05:32:29  m1378502
;; Fixed problem about suppressing item even in writing sub-entry.
;;
;; Split function clmemo-entry to clmemo-get-entry and clmemo-insert-entry.
;; (clmemo-get-entry, clmemo-insert-entry): New function.
;; (clmemo-entry, clmemo-change-case): Removed.
;;
;; Revision 1.15  2002/09/04 07:56:24  m1378502
;; Ignore case when completing entry.
;; Fixed bug in previous change.
;;
;; Revision 1.14  2002/09/04 06:24:25  m1378502
;; Suppress new item, when keyboard-quit.
;; Patch by rubikitch <rubikitch@ruby-lang.org>
;;
;; Revision 1.13  2002/07/01 11:48:42  m1378502
;; Fixed bug that insert emacs/w3m info correctly.
;;
;; Revision 1.12  2002/06/29 06:56:44  m1378502
;; Fixed typo.
;;  Quetes " in the doc string.
;;
;; Revision 1.11  2002/06/28 17:10:50  m1378502
;; Cars of clmemo-entry-alist is dummy, and cdrs are the true.
;; Just string is acceptable, and mixing them is a good idea.
;; New func: clmemo-completing-read
;;
;; Revision 1.10  2002/06/27 08:08:33  m1378502
;; Provide clmemo.
;;
;; Revision 1.9  2002/06/26 16:14:32  Mark
;; Doc fix.
;; Add doc for function clmemo-entry and clmemo-change-case.
;; Also capitalize subentry, depending on var clmemo-entry-upper-case.
;;
;; Revision 1.8  2002/06/20 19:17:04  m1378502
;; Add doc about setting key-binding.
;; Fix code.
;; New func: clmemo-change-case and clmemo-www-item-insert
;;  Split from clmemo-entry.
;;
;; Revision 1.7  2002/05/30 18:54:46  m1378502
;; Doc fix: clmemo is called interactively.
;;
;; Revision 1.6  2002/05/17 06:22:34  m1378502
;; Support subentry.
;; New var: clmemo-subentry-char
;;  If you add clmemo-subentry-char end of the entry,
;;  clmemo ask subentry.
;;
;; Revision 1.5  2002/05/14 05:14:35  m1378502
;; Add doc about file variables in comment section.
;;
;; Revision 1.4  2002/05/12 21:57:54  m1378502
;; Fixed bug of completing case sensitive.
;;
;; Revision 1.3  2002/05/12 13:19:54  m1378502
;; Fixed typo.
;; Fixed bug that clmemo with prefix arg also call clmemo-entry.
;; Collaborate with emacs-w3m; ask inserting url and title.
;;
;; Revision 1.2  2002/05/11 09:13:54  m1378502
;; Doc fix and code shape up.
;; Add Log.
;; Set fill column 72.
;; Open clmemo-file-name when clmemo called with prefix argument, even if
;; clmemo file is closed.
;;

;;; Code:
(provide 'clmemo)
(require 'add-log)

(autoload 'clgrep "clgrep" "grep mode for ChangeLog file." t)
(autoload 'clgrep-title "clgrep" "grep first line of entry in ChangeLog." t)
(autoload 'clgrep-header "clgrep" "grep header line of ChangeLog." t)
(autoload 'clgrep-clmemo "clgrep" "clgrep directly ChangeLog MEMO." t)

;;
;; User Options
;;
(defvar clmemo-entry-upper-case nil
  "*If nil, case insensitive for entry text.
If t, capitalize the whole entry text.
If 1, capitalize initial letter.")

(defvar clmemo-file-name "~/clmemo.txt"
  "*Clmemo file name.")

(defvar clmemo-entry-list
  '("idea" "system" "url")
  "*List of entries.
Set your favourite entry of ChangeLog MEMO.

It is possible to mix strings and cons-cells.
For example:
(setq clmemo-entry-list
  (\"url\" (\"clmemo\" . \"ChangeLog MEMO\")))
Cars are dummy and cdrs are the true entry.
This case, clmemo is a dummy entry of ChangeLog MEMO.")

(defvar clmemo-subentry-char "+"
  "*If this char is in the end of entry, ask subentry.")

(defvar clmemo-subentry-punctuation-char '(" (" . ")")
  "*Car is left string of subentry punctuation char; Cdr is right.")

(defvar clmemo-schedule-header "[s]"
  "Header string for schedule.")

(defvar clmemo-time-string-with-weekday nil
  "*If non-nil, append weekday after date.")

(defvar clmemo-indent-only-one-tab nil
  "*If non-nil, `clmemo-indent-region' indent with only one tab.")

;;;###autoload
(defvar clmemo-mode nil
  "Toggle clmemo-mode.")
(make-variable-buffer-local 'clmemo-mode)

(unless (assq 'clmemo-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(clmemo-mode " MEMO") minor-mode-alist)))

;;;###autoload
(defun clmemo (arg)
  "Open ChangeLog memo file and ask entry.

With prefix argument ARG, just open ChangeLog memo file.
 If already visited the ChangeLog memo file,
 ask entry and insert it in the date where point is
Prefix argument twice (C-u C-u), then call `clgrep-clmemo'.
Three times (C-u C-u C-u), then `clgrep-clmemo' in reverse order.
C-u 0, toggle open other window and call `clgrep-clmemo'.
C-u 1, toggle open other window and call `clgrep-clmemo' in reverse order.

For ChangeLog memo file, use var `clmemo-file-name'.
See also `add-change-log-entry' and `clmemo-get-entry'."
  (interactive "P")
  (let ((add-log-time-format (if clmemo-time-string-with-weekday
				 'add-log-iso8601-time-string-with-weekday
			       'add-log-iso8601-time-string))
	(file (expand-file-name clmemo-file-name)))
    (cond
     ;; With no prefix argument.
     ((not arg)
      (let ((buf-orig (current-buffer))
	    (entry    (clmemo-get-entry)))
	(add-change-log-entry-other-window nil file)
	(clmemo-mode)
	(clmemo-insert-entry buf-orig entry)))
     ;; C-u C-u C-u
     ((equal arg '(64))
      (clgrep-clmemo))
     ;; C-u C-u
     ((equal arg '(16))
      (setq current-prefix-arg nil)
      (clgrep-clmemo))
     ;; C-u 0  -- Toggle other window
     ((equal arg 0)
      (clgrep-clmemo))
     ;; C-u 1  -- Toggle other window in reverse order.
     ((equal arg 1)
      (clgrep-clmemo))
     ;; C-u
     (arg
      (if (equal buffer-file-name file)
	  (let ((buf-org (current-buffer))
		(entry   (clmemo-get-entry)))
	    (re-search-backward "^[12][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]  " nil t)
	    (forward-line 1)
	    (insert "\n\t* DUMMY: \n")
	    (backward-char 1)
	    (clmemo-insert-entry buf-org entry))
	(if (get-file-buffer file)
	    (switch-to-buffer (get-file-buffer file))
	  (switch-to-buffer (find-file-noselect file))))))))

(defun clmemo-mode (&optional arg)
  "Minor mode to insert entry with completion for ChangeLog MEMO.
For more information, See function `clmemo'.

\\{clmemo-mode-map}
"
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      (setq clmemo-mode nil)
    (setq clmemo-mode t)))

(defun clmemo-get-entry ()
  "Ask entry of ChangeLog MEMO and return string of entry.

If the last char of the entry is `clmemo-subentry-char',
also ask the subentry.

If `clmemo-entry-upper-case' is t, capitalize whole entry.
If 1, capitalize initial letter of entry.
If nil, do nothing."
  (let ((entry (clmemo-completing-read "clmemo entry: ")))
    (when (and (not (string= "" entry))
	       (clmemo-subentry-p entry))
      (let* ((main  (substring entry 0 (- (length clmemo-subentry-char))))
	     (sub   (clmemo-completing-read (format "subentry for %s: " main)))
	     (left  (car clmemo-subentry-punctuation-char))
	     (right (cdr clmemo-subentry-punctuation-char)))
	(setq entry (concat main left sub right))))
    entry))

(defun clmemo-insert-entry (buf-orig entry)
  "Insert ChangeLog memo entry."
  (save-excursion
    (let ((eol (progn (end-of-line 1) (point))))
      (unless (string= "" entry)
	(forward-line 0)
	(if (re-search-forward "^\\s +\\* \\(.+\\):" eol t)
	    (replace-match entry t nil nil 1)
	  (end-of-line 1)
	  (insert entry ": ")))))
  (end-of-line 1)
  (when (fboundp 'clkwd-insert-url)
    (clkwd-insert-url (clkwd-assoc-format 'clkwd-insert-url 
					  clkwd-insert-function-alist)
		      buf-orig t)))


(defun clmemo-completing-read (prompt)
  "Read a string in the minibuffer, with completion using `clmemo-entry-list'.

PROMPT is a string to prompt with; normally it ends in a colon and space."
  (let* ((completion-ignore-case t)
	 (alist (mapcar (lambda (x) (if (consp x) x (cons x x)))
			clmemo-entry-list))
	 (item  (completing-read prompt alist))
	 (subp  (clmemo-subentry-p item))
	 (cell  (if subp
		    (assoc (substring item 0 (- (length clmemo-subentry-char))) alist)
		  (assoc item alist)))
	 (entry (or (cdr cell)
		    (if subp
			(substring item 0 (- (length clmemo-subentry-char)))
		      item))))
    ;; Change case of entry.
    (setq entry 
	  (cond
	   ;; Capitalize whole entry text.
	   ((equal clmemo-entry-upper-case t)
	    (upcase entry))
	   ;; Capitalize initial letter of the entry.
	   ((equal clmemo-entry-upper-case 1)
	    (capitalize entry))
	   ;; As it is.
	   (t entry)))
    ;; Add subentry suffix if needed.
    (if subp
	(concat entry clmemo-subentry-char)
      entry)))
  

(defun clmemo-subentry-p (entry)
  "Return t if argument ENTRY has subentry suffix.
Subentry suffix is defined in variable `clmemo-subentry-char'."
  (and clmemo-subentry-char
       (string= clmemo-subentry-char
		(substring entry (- (length clmemo-subentry-char))))))



;;
;; Header with or without weekday 
;;

;; Code contributed by Satoru Takabayashi <satoru@namazu.org>
(defun add-log-iso8601-time-string-with-weekday ()
  (let ((system-time-locale "C"))
    (concat (add-log-iso8601-time-string)
            " " "(" (format-time-string "%a") ")")))


(defun clmemo-format-header-with-weekday (beg end)
  "Format ChangeLog header with weekday
FROM: 2001-01-01  ataka
TO:   2001-01-01 (Mon)  ataka

See also function `clmemo-format-header-without-weekday'."
  (interactive "r")
  (let* ((system-time-locale "C")
	 date weekday)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\([-0-9]+\\)" end t)
	(save-match-data
	  (setq date    (match-string 0)
		weekday (format-time-string "%a" (date-to-time (concat date " 0:0:0")))))
	(replace-match (concat date " (" weekday ")"))))))

(defun clmemo-format-header-without-weekday (beg end)
  "Format ChangeLog header without weekday
FROM:   2001-01-01 (Mon)  ataka
TO:     2001-01-01  ataka

See also function `clmemo-format-header-with-weekday'."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\([-0-9]+\\) (.+)" end t)
      (replace-match "\\1"))))



;;
;; ChangeLog MEMO Mode
;;

(defvar clmemo-mode-map nil)
(if clmemo-mode-map
    nil
  (let ((map (make-keymap)))
    ;; clgrep
    ;; Movement
    (define-key map "\C-c\C-f" 'forward-paragraph)
    (define-key map "\C-c\C-b" 'backward-paragraph)
    (define-key map "\C-c\C-n" 'forward-page)
    (define-key map "\C-c\C-p" 'backward-page)
    ;; yank and indent
    (define-key map "\C-c\C-y" 'clmemo-yank)
    (define-key map "\C-c\C-i" 'clmemo-indent-region)
    ;; Schedule
    (define-key map "\C-c\C-c" 'clmemo-schedule)
    (setq clmemo-mode-map map)))

(eval-after-load 'clgrep
  '(progn
     (define-key clmemo-mode-map "\C-c\C-f" 'clgrep-forward-entry)
     (define-key clmemo-mode-map "\C-c\C-b" 'clgrep-backward-entry)))

(unless (assq 'clmemo-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'clmemo-mode clmemo-mode-map) minor-mode-map-alist)))


(defun clmemo-yank (&optional arg)
  "Yank with indent.

With prefix argument, Point returns the point where start yank.
Do not indent when `yank-pop'."
  (interactive "P")
  (unless (bolp)
    (backward-char 1)
    (if (looking-at "^\t")
	(delete-char 1)
      (forward-char 1)))
  (let ((beg (point))
        (end (progn (yank) (point))))
    (clmemo-indent-region beg end)
    (when arg
      (goto-char beg))))

(defun clmemo-indent-region (beg end)
  "Indent region with one TAB.

If `clmemo-indent-only-one-tab' is non-nil, do not indent line
that is already indented by tabs."
  (interactive "r*")
  (save-excursion
    (goto-char beg)
    (forward-line 0)
    (while (< (point) end)
      (unless (and clmemo-indent-only-one-tab 
		   (equal (following-char) ?\C-i))
	(insert "\t"))
      (forward-line 1))))

(defun clmemo-schedule ()
  "Insert schedule flags and puts date string.
See variable `clmemo-schedule-header' for header flag string."
  (interactive)
  (backward-paragraph)
  (forward-line 1)
  (search-forward "\t* " nil t)
  (insert clmemo-schedule-header)
  (search-forward ": ")
  (insert "[] ")
  (backward-char 2))


(if (locate-library "clkwd")
    (require 'clkwd))
;;; clmemo.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
