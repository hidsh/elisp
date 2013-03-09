;;; clgrep.el --- Change Log grep -*-emacs-lisp-*-

;; Copyright (c) 2002 Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; $Id: clgrep.el,v 1.54 2003/07/14 17:52:45 m1378502 Rel $

;; Author: Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; Keywords: tools, convenience

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

;; clgrep is grep mode especially for ChangeLog file.

;; clgrep runs grep for visiting ChangeLog file and collect output in a
;; buffer, order by date.  Called with prefix argument, output is in
;; reverse order.  Use regexp for lookup string.

;; Use function clgrep-title to grep only for the title of entry --- the
;; first line of the entry,
 
;; Collected Outupt is in *clgrep* buffer.  Move cursor with `n' and `p'
;; or `f' and `b'.  Like *grep* buffer, hit RET in *clgrep* buffer to
;; visit the original ChangeLog file where point is.

;; [latest clgrep.el]:
;;
;;   http://pop-club.hp.infoseek.co.jp/emacs/clgrep.el

;; [Thanks]:
;;
;; Great thanks to Shun-ichi Goto, Yuuji Hirose, Hideyuki Shirai,
;; Keiichi Suzuki, and ELF ML members for all their help.

;;; How to install:

;; To install, put this in your .emacs file:
;;
;;   (autoload 'clgrep "clgrep" "grep mode for ChangeLog file." t)
;;   (autoload 'clgrep-title "clgrep" "grep first line of entry in ChangeLog." t)
;;   (autoload 'clgrep-header "clgrep" "grep header line of ChangeLog." t)
;;   (autoload 'clgrep-other-window "clgrep" "clgrep in other window." t)
;;

;; This is key-bind example for ChangeLog mode:
;;
;;   (add-hook 'change-log-mode-hook
;;        '(lambda ()
;;           (define-key change-log-mode-map "\C-c\C-g" 'clgrep)
;;           (define-key change-log-mode-map "\C-c\C-t" 'clgrep-title)))
;;

;;; Bugs:

;; - M-x clgrep-goto-line can't find correct line if the ChangeLog file is
;; modified after clgrep.


;;; ChangeLog:

;; $Log: clgrep.el,v $
;; Revision 1.54  2003/07/14 17:52:45  m1378502
;; (clgrep-header-regexp): supports weekend in the header.
;;
;; Revision 1.53  2003/06/04 11:27:40  m1378502
;; (clgrep-lookup): Fixed bug that entering permanent loop.
;; If file names are enumerated without blank lines,
;; clgrep enters permanent loop.
;;
;; Revision 1.52  2003/05/19 16:39:32  m1378502
;; Change URL.
;;
;; Revision 1.51  2003/05/13 23:24:18  m1378502
;; (clgrep-lookup): Fixed previous change.
;; Get max point of buffer if lookup text is found in the last entry.
;;
;; Revision 1.50  2003/04/30 14:59:29  m1378502
;; (clgrep-lookup): split text from changelog even though the text includes open line.
;; Regard text separater as "^[TAB]*".  So changelog entry must contains "*" at beginning.
;;
;; Revision 1.49  2002/12/20 22:40:43  m1378502
;; (clgrep-goto-line): Fix typo.
;;
;; Revision 1.48  2002/12/20 22:05:32  m1378502
;; (clgrep-date-face, clgrep-keyword-face): Fix typo.
;; Reported by TSURUDA Naoki <tsuru@yokohama.office.ne.jp>
;;
;; Revision 1.47  2002/12/16 22:31:21  m1378502
;; Version 0.8.0 -- Work with clkwd.el
;; Move provide code head of file.
;; Load clkwd.el if found.
;; (clgrep-jump, clgrep-goto-date, clgrep-jump-other-window)
;; (clgrep-goto-keyword-quickly, clgrep-goto-keyword)
;; (clgrep-goto-keyword-url, clgrep-goto-keyword-url-backward)
;; (clgrep-keyword-help, clgrep-regexp-date-or-keyword)
;; (clgrep-next-date-or-keyword, clgrep-previous-date-or-keyword): Move to clkwd.el
;;
;; Revision 1.46  2002/10/13 10:51:49  m1378502
;; (clgrep-lookup): Set clgrep buffer disable to undo.
;;
;; Revision 1.45  2002/10/02 23:30:28  m1378502
;; (clgrep-quit-delete-window): New func.
;;
;; Revision 1.44  2002/10/02 11:37:45  m1378502
;; Toggle open other window.
;; (clgrep-open-other-window): New variable.
;; (clgrep-other-window): New function.
;;
;; (clgrep-clmemo): Do not call clmemo but just open clmemo-file.
;; (clgrep-goto-date): Goto date and put cursor on the top of window.
;;
;; Revision 1.43  2002/09/24 07:26:03  m1378502
;; (May be) speed up.
;; Rename function from clgrep-out-of-header-p to clgrep-inside-header-p.
;; Require add-log before use change-log-font-lock-keywords.
;; (clgrep-insert-output): Removed
;; (clgrep-copy-to-register): Fixed typo.
;; (clgrep-goto-keyword-url-backward): New function.  Bind to 'U'.
;;
;; Revision 1.42  2002/09/17 16:26:24  m1378502
;; Run grep for the ChangeLog header.
;; (clgrep-header): New func.
;;
;; Revision 1.41  2002/09/17 13:02:36  m1378502
;; Doc fix.
;; (clgrep-buffer): Fix variable bufname.
;;
;; Revision 1.40  2002/09/16 09:07:11  m1378502
;; Do not clgrep after clgrep-text-delimiter.
;; (clgrep-text-delimiter): New var.
;;
;; Revision 1.39  2002/09/04 07:38:59  m1378502
;; Function clgrep-lookup now select window correctly.
;; Reported by rubikitch <rubikitch@ruby-lang.org>
;;
;; Revision 1.38  2002/07/23 11:06:40  m1378502
;; New func: clgrep-jump-other-window
;;
;; Revision 1.37  2002/07/23 09:24:14  m1378502
;; Support jump to date when point is on the date [20xx-??-??].
;; TAB move point to date or keyword.
;; New func: clgrep-jump, clgrep-goto-date
;; New func: clgrep-next-date-or-keyword, clgrep-previous-date-or-keyword
;; New func: clgrep-regexp-date-or-keyword
;;
;; Revision 1.36  2002/07/23 05:35:48  m1378502
;; Font Lock date and keywords.
;; New face: clgrep-date-face, clgrep-keyword-face
;; New var: clgrep-date-face, clgrep-keyword-face
;; New var: clgrep-font-lock-keywords
;; Add document about key binding.
;;
;; Revision 1.35  2002/07/22 23:27:08  m1378502
;; Support keyword search.
;; New var: clgrep-keyword-alist.
;; New func: clgrep-goto-keyword, clgrep-goto-keyword-quickly, clgrep-goto-keyword-url
;; New func: clgrep-keyword-help.
;;
;; Revision 1.34  2002/06/28 06:35:59  m1378502
;; Fixed typo.
;;
;; Revision 1.33  2002/06/27 16:23:36  m1378502
;; Function clgrep-buffer use abbreviate-file-name.
;;
;; Revision 1.32  2002/06/27 15:15:07  m1378502
;; Do not count lines.
;; Trust ChangeLog file not to have duplicated header line.
;;
;; Revision 1.31  2002/06/27 13:41:46  m1378502
;; ChangeLog file path is showed in mode-line; instead beginning of buffer.
;;  Buffer name include ChangeLog file path.
;; Function clgrep and clgrep-title is able to call from other function.
;; Function clgrep-lookup is now root function of clgrep.
;;  Function clgrep-nest-lookup call it internally.
;; Function clgrep-noquery make buffer and copy current buffer.
;;  Now do not use original text.
;; Function clgrep-kill-ring-save and clgrep-copy-to-register make temp buffer,
;;  do not narrow-to-region and edit buffer text.
;; Add provide clgrep.
;;
;; New func: clgrep-clmemo
;; New func: clgrep-buffer, clgrep-file-name
;; Remove Func: clgrep-back-to-clmemo
;;
;; Revision 1.30  2002/06/27 10:41:59  m1378502
;; *** empty log message ***
;;
;; Revision 1.29  2002/06/27 07:19:50  m1378502
;; Rewrite clgrep-(forward|backward)-entry.
;;  Use regexp search instead of forward-paragraph.
;;
;; Revision 1.28  2002/06/26 23:40:23  m1378502
;; Print how many entries are matched.
;; Rename function name from clgrep-out-header-p to clgrep-out-of-header-p,
;;  and doc fix.
;;
;; Revision 1.27  2002/06/25 11:09:31  m1378502
;; Doc fix.
;; `-' is negative-argument.
;;
;; Revision 1.26  2002/06/25 08:49:55  m1378502
;; Fixed bug, copy-to-register's argument was wrong.
;; `h' key marks paragraph.
;; M-w and C-r i s substitute by clgrep function.
;;
;; Revision 1.25  2002/06/25 18:07:37  Mark
;; Copy with removing TAB in the beginning of the line.
;; New func: clgrep-kill-ring-save, clgrep-copy-to-register
;; New func: clgrep-copy
;;
;; Revision 1.24  2002/06/25 16:42:58  Mark
;; Cause error and do nothing if query string do not match anything.
;;
;; Revision 1.23  2002/06/21 20:25:01  m1378502
;; Support highlight match string.
;; New func: clgrep-match-highlight
;; New var: clgrep-highlight-match-string
;; New var: clgrep-highlight-match-face
;;
;; Revision 1.22  2002/06/21 17:11:20  m1378502
;; New var: clgrep-output-buffer
;; New func: clgrep-quit.
;; New func: clgrep-back-to-clmemo
;;
;; Now function clgrep-noquery do not make new buffer,
;; but rename buffer name of clmemo file.
;;
;; Revision 1.21  2002/06/21 15:52:10  m1378502
;; Function clgrep-noquery do not count lines.
;;
;; Revision 1.20  2002/06/21 15:25:07  m1378502
;; New hook: clgrep-mode-hook
;;
;; Revision 1.19  2002/06/21 15:17:20  m1378502
;; Add a lot of commands to clgrep-mode-map.
;; moving, searching, copying, etc...
;;
;; Revision 1.18  2002/06/21 12:57:12  m1378502
;; Doc fix.
;;  Print mode help with clgrep-mode document.
;;
;; Revision 1.17  2002/06/19 00:09:38  m1378502
;; Fixed bug in clgrep-nest-lookup.
;;  Call tmpbuf as clbuf.
;;
;; Revision 1.16  2002/05/30 18:55:16  m1378502
;; Doc fix: clgrep is called interactively.
;;
;; Revision 1.15  2002/05/24 05:32:24  m1378502
;; Case sensitive like occur.
;; Set var case-fold-search.
;;
;; Revision 1.14  2002/05/23 07:54:12  m1378502
;; Change format of clgrep.
;; Insert a blank line between header and entry.
;; New func: clgrep-forward-entry, clgrep-backward-entry
;;  Move beginning of forward and backward entry.
;; New var: clgrep-noquery
;;  Check var;
;; Remove var: clgrep-entry-header-regexp
;; Remove var: clgrep-entry-delimiter
;;  Use function of moving paragraph.
;;
;; Revision 1.13  2002/05/17 04:49:25  m1378502
;; Do not search in clgrep-header.
;; Fixed bug that go to infinite loop when clgrep-header contains query.
;; New func: clgrep-out-header-p
;;  Return t if point is not in clgrep-header.
;;
;; Revision 1.12  2002/05/17 02:52:15  m1378502
;; Use insert instead of insert-string.
;; Now [backspace] is scroll-down in clgrep-mode.
;;
;; Revision 1.11  2002/05/17 02:30:51  m1378502
;; New func: clgrep-noquery
;;  If query string is empty, do nothing but get into clgrep-mode.
;;  Then, reverse-order option is ignored.
;;
;; Revision 1.10  2002/05/14 09:20:03  m1378502
;; Support clgrep-title.
;; New func: clgrep-title
;;  Interface for grep each title of entry in ChangeLog.
;; New func: clgrep-output-p
;;  Return t if POS is in adequate region, using clgrep-lookup-method.
;; New var: clgrep-lookup-method
;;  To tell clgrep the region for lookup.
;; Some doc fix.
;; Rename var name from change-log-* to clgrep-*.
;;
;; Revision 1.9  2002/05/14 06:09:18  m1378502
;; Remove useless code and reindent.
;;
;; Revision 1.8  2002/05/14 01:49:51  m1378502
;; New func: clgrep-insert-output.
;;  Common code of clgrep-lookup and clgrep-nest-lookup.
;; Remove clgrep-init.  Get into clgrep-lookup.
;; Use backward-paragraph if failed find change-log-entry-header-regexp.
;;
;; Revision 1.7  2002/05/12 22:14:25  m1378502
;; Remove useless code. (shape up)
;;
;; Revision 1.6  2002/05/11 04:59:38  m1378502
;; Do not set var version-control and adaptive-fill-regexp for buffer
;; unwritable.
;; Remove function clgrep-{next,previous,forward}-entry.
;; Use {forward,backward}-page instead.
;;
;; Revision 1.5  2002/05/11 04:37:46  m1378502
;; Doc fix.
;; Add New comment entry: "Bugs".
;; Add information of latest clgrep.el.
;; Set fill column 72.
;;
;; Revision 1.4  2002/05/11 04:16:08  m1378502
;; clgrep-goto-line find ChangeLog file and go to the entry if the
;; ChangeLog file is closed.
;;
;; Revision 1.3  2002/05/09 16:50:23  m1378502
;; Doc fix.
;; Rename var name from clmemo-* to change-log-*.
;;
;; Revision 1.2  2002/05/09 16:24:21  m1378502
;; Divid clgrep function.
;; Can clgrep recursively in *clgrep* buffer and bind clgrep to `g'.
;; Call clgrep-goto-line if typed RET in *clgrep*.
;;

;;; Code:
(provide 'clgrep)

;;
;; User Options.
;;
(defvar clgrep-mode-hook nil
  "*Normal hook run when staring to view *clgrep* buffer.")

(defvar clgrep-keyword-alist
    '(("url" . "<url: "))
  "*Alist of keyword and regexp.")

(defvar clgrep-highlight-match-string (fboundp 'overlay-put)
  "*Controls the highliting matched text in *clgrep* buffer.")

(defvar clgrep-header-regexp "^[12][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9] [ (]"
  "*Regexp for a header of ChangeLog.")

(defvar clgrep-text-delimiter "^"
  "*Regexp for a ChangeLog.
Do not grep after the delimiter.
You can write File Variables or text after it.")

(defvar clgrep-open-other-window t
  "*If nil, clgrep does not open another window.")


(defvar clgrep-output-buffer "*clgrep: %s*"
  "Output buffer name of clgrep.
%s is file name of ChangeLog.")

(defvar clgrep-lookup-method 'entry
  "Keyword of lookup method.
Keywords are 'entry', 'title', and 'header'.")

(defvar clgrep-noquery nil)

;; Set Face and Font Lock
(when (fboundp 'defface)
  (defface clgrep-date-face
    '((((class color) (background light))
       (:foreground "slateblue"))
      (((class color) (background dark))
       (:foreground "yellow"))
      (t
       :bold t))
    "Face for highlighting date.")

  (defface clgrep-keyword-face
    '((((class color) (background light))
       (:foreground "dimgray"))
      (((class color) (background dark))
       (:foreground "yellow"))
      (t
       :bold t))
    "Face for highlighting keyword.")

  (defface clgrep-highlight-match-face nil
    "Face for highlighting the match string in *clgrep* buffer.")

  (defvar clgrep-date-face 'clgrep-date-face)
  (defvar clgrep-keyword-face 'clgrep-keyword-face)
  (defvar clgrep-highlight-match-face 'clgrep-highlight-match-face)

  (defvar clgrep-font-lock-keywords
    `(;;
      ;; Date
      ("\\[[0-9-]+\\]" (0 'clgrep-date-face))
      ;; Keywords
      ,@(mapcar '(lambda (regexp)
                   (list (concat regexp ".+$") '(0 'clgrep-keyword-face)))
                (mapcar '(lambda (cell)
                           (cdr cell))
                        clgrep-keyword-alist)))
    "Additional expressions to highlight in clgrep mode.")

  (if (boundp 'isearch-lazy-highlight-face)
      (copy-face 'isearch-lazy-highlight-face 'clgrep-highlight-match-face)
    (copy-face 'secondary-selection 'clgrep-highlight-match-face))
)


;;;###autoload
(defun clgrep (query &optional arg)
  "Run grep for ChangeLog file and collect ouput in a buffer.
With prefix argument (C-u), output is in reverse order.
C-u 0: Toggle open other window, see `clgrep-open-other-window'.
C-u 1: Toggle open other window and in reverse order.

If query string is empty, do nothing but get into `clgrep-mode'.
Then, reverse-order option is ignored.

Use function `clgrep-title' to restrict region for grep; only for the
first line of each entry."
  (interactive "sChangeLog grep regexp: \nP")
  (if (string= "" query)
      (unless (eq major-mode 'clgrep-mode)
        (clgrep-noquery))
    ;; When query is not empty.
    (save-excursion
      (goto-char (point-min))
      ;; Fix the case of query only in header.
      (unless (re-search-forward query nil t)
        (error "No matchs: %s" query)))
    (if (eq major-mode 'clgrep-mode)
        (clgrep-nest-lookup arg query)
      (clgrep-lookup arg query (clgrep-buffer)))
    (clgrep-mode)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t
          clgrep-noquery   nil)
    (when clgrep-highlight-match-string
      (clgrep-match-highlight query))))

;;;###autoload
(defun clgrep-title ()
  "Run grep for the first line of each entry and collect output in a buffer.
With prefix argument, output in reverse order.

Call function `clgrep' internally."
  (interactive)
  (let ((clgrep-lookup-method 'title))
    (call-interactively 'clgrep)))

;;;###autoload
(defun clgrep-header ()
  "Run grep for the ChangeLog header.
With prefix argument, output in reverse order.

Call function `clgrep' internally."
  (interactive)
  (let ((clgrep-lookup-method 'header))
    (call-interactively 'clgrep)))

;;;###autoload
(defun clgrep-other-window ()
  "Run clgrep in another window.

If `clgrep-open-other-window' is t, do not open window."
  (let ((clgrep-open-other-window (not clgrep-open-other-window)))
    (call-interactively 'clgrep)))

;;;###autoload
(defun clgrep-clmemo ()
  "clgrep directly ChangeLog MEMO."
  (interactive)
  (require 'clmemo)
  (set-buffer (find-file-noselect clmemo-file-name))
  (call-interactively 'clgrep))


(defun clgrep-lookup (rev query clbuf)
  "Internal code for clgrep ChangeLog file."
  ;; clean clbuf.
  (save-excursion
    (set-buffer clbuf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (if (fboundp 'buffer-disable-undo)
	(buffer-disable-undo)
      (buffer-flush-undo))
    (goto-char (point-min)))
  ;; lookup and switch buffer.
  (let ((case-fold-search (and case-fold-search
                               (isearch-no-upper-case-p query t)))
        (count  0)
	(clwin  (get-buffer-window clbuf))
	(other-window clgrep-open-other-window))
    ;; C-u 0 / 1: upset option clgrep-other-window.
    (when (numberp rev)
      (cond
       ((equal rev 0)
	(setq rev nil
	      other-window (not other-window)))
       ((equal rev 1)
	(setq other-window (not other-window)))))
    (save-excursion
      (save-restriction
	(narrow-to-region (goto-char (point-min))
			  (or (re-search-forward clgrep-text-delimiter nil t)
			      (point-max)))
	(let* ((clgrep-memo-separate "[ \t\n]+\n\t\\*")
	       ebeg eend entry              ;entry  beginning, end, and itself
	       hbeg hend header             ;header beginning, end, and itself
	       (clgrep-header-p (equal clgrep-lookup-method 'header))
	       ;; Local functions.
	       clgrep-search
	       clgrep-print-p
	       clgrep-entry-beg
	       clgrep-entry-end)
	  ;; Define Local Functions.
	  (if rev
	      (setq clgrep-search 're-search-backward)
	    (setq clgrep-search 're-search-forward))
	  (if (equal clgrep-lookup-method 'header)
	      ;; Header
	      (setq clgrep-print-p  'clgrep-inside-header-p
		    clgrep-entry-beg (lambda () (forward-line 1) (point))
		    clgrep-entry-end (lambda () (forward-page 1) (forward-line -1) (point)))
	    ;; Memo or title
	    (setq clgrep-print-p   (lambda () (not (clgrep-inside-header-p)))
		  clgrep-entry-beg (lambda () (re-search-backward clgrep-memo-separate nil t)
				     (skip-chars-forward " \t\n")
				     (forward-line 0) (point))
		  clgrep-entry-end (lambda ()
				     (if (re-search-forward clgrep-memo-separate nil t)
					 (progn
					   (forward-line -2)
					   (if (clgrep-inside-header-p)
					       (forward-line -1)
					     (forward-line 1)))
				       (goto-char (point-max))
				       (backward-char 2))
				     (point))))
	  ;; lookup and insert
	  (goto-char (if rev (point-max) (point-min)))
	  (while (funcall clgrep-search query nil t)
	    (when (funcall clgrep-print-p)
	      (setq count (1+ count))
	      ;; insert entry and header
	      (let ((pos (point)))
		(forward-line 1)
		(setq ebeg (funcall clgrep-entry-beg)
		      eend (funcall clgrep-entry-end)
		      hbeg (re-search-backward clgrep-header-regexp nil t)
		      hend (progn (end-of-line 1) (point)))
		(when (clgrep-output-p pos (if clgrep-header-p hbeg ebeg))
		  (save-excursion
		    (setq entry  (buffer-substring ebeg eend)
			  header (buffer-substring hbeg hend))
		    (set-buffer clbuf)
		    (insert header "\n\n"
			    entry  "\n")))
		(goto-char (if clgrep-header-p
			       (if rev hbeg hend)
			     (if rev ebeg eend)))))))))
    (if clwin
	(progn
	  (select-window clwin)
	  (switch-to-buffer clbuf))
      (if other-window
	  (pop-to-buffer clbuf)
	(switch-to-buffer clbuf)))
    (goto-char (point-min))
    (message "%s matches." count)))

(defun clgrep-nest-lookup (rev query)
  "Internal code for clgrep *clgrep* buffer recursively."
  (let ((buf  (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (clgrep-mode)
      (clgrep-lookup rev query (get-buffer buf)))))


(defun clgrep-inside-header-p ()
  "Return t if point is inside `clgrep-header-regexp'.

Make sure that lookup string is in clgrep-header:
for example, date and e-mail address in header"
  (let ((pos (point))
        beg end)
    (save-excursion
      (forward-line 1)
      (setq beg (re-search-backward clgrep-header-regexp nil t)
            end (progn (end-of-line 1) (point))))
    (and (<= beg pos) (<= pos end))))

(defun clgrep-output-p (pos beg)
  "Return t if arg POS is in the adequate region.

The adequate region depend on var `clgrep-lookup-method'."
  (let ((end (save-excursion (goto-char beg) (end-of-line 1) (point))))
    (or (equal clgrep-lookup-method 'entry)
	(and (<= beg pos) (<= pos end)))))

(defun clgrep-buffer ()
  "Return buffer of clgrep output."
  (let ((file (if (eq major-mode 'clgrep-mode)
                  (expand-file-name (clgrep-file-name))
                (buffer-file-name)))
	bufname)
    (if (and (boundp 'clmemo-file-name)
             (string= file (expand-file-name clmemo-file-name)))
        (setq bufname "CLMEMO")
      (setq bufname (abbreviate-file-name file)))
    (get-buffer-create (format clgrep-output-buffer bufname))))

(defun clgrep-file-name ()
  "Return file name of ChangeLog."
  (let* ((bufname (buffer-name))
         (beg     (string-match "%s" clgrep-output-buffer))
         (file    (substring bufname beg -1)))
    (if (string= file "CLMEMO")
        clmemo-file-name
      (expand-file-name file))))
    
(defun clgrep-noquery ()
  "Switch to clgrep-mode.

Do not support reverse order by date."
  (let ((pos (point))
        (buf (current-buffer)))
    (switch-to-buffer (clgrep-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-buffer-substring buf)
    (goto-char pos)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t
          clgrep-noquery   t)
    (clgrep-mode)))


(defun clgrep-match-highlight (search)
  "Highlight match string.
Variable `clgrep-highlight-match-string' controls highlight or not.
Highlight face uses isearch-lazy-highlight-face"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward search nil t)
      (let ((mb (match-beginning 0))
            (me (match-end 0)))
        (if (= mb me)                   ;zero-length match
            (forward-char 1)

          ;; non-zero-length match
          (let ((ov (make-overlay mb me)))
            (overlay-put ov 'face clgrep-highlight-match-face)
            (overlay-put ov 'prioryty 0) ;lower than main overlay
            (overlay-put ov 'window (selected-window))))))))


;;;
;;; clgrep mode
;;;

(defvar clgrep-mode-map nil)
(if clgrep-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    ;; digit arguments
    (define-key map "0" 'digit-argument)
    (define-key map "1" 'digit-argument)
    (define-key map "2" 'digit-argument)
    (define-key map "3" 'digit-argument)
    (define-key map "4" 'digit-argument)
    (define-key map "5" 'digit-argument)
    (define-key map "6" 'digit-argument)
    (define-key map "7" 'digit-argument)
    (define-key map "8" 'digit-argument)
    (define-key map "9" 'digit-argument)
    (define-key map "-" 'negative-argument)
    (define-key map "q" 'clgrep-quit)
    (define-key map "Q" 'clgrep-quit-delete-window)
    ;; clgrep commands
    (define-key map "g" 'clgrep)
    (define-key map "t" 'clgrep-title)
    (define-key map "H" 'clgrep-header)
    ;; movement functions
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    (define-key map " " 'scroll-up)
    (define-key map [backspace] 'scroll-down)
    (define-key map "n" 'forward-page)
    (define-key map "p" 'backward-page)
    (define-key map "f" 'clgrep-forward-entry)
    (define-key map "b" 'clgrep-backward-entry)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    (define-key map "a" 'back-to-indentation)
    (define-key map "e" 'end-of-line)
    (define-key map "l" 'recenter)
    ;; copy commands
    (define-key map "." 'set-mark-command)
    (define-key map "h" 'mark-paragraph)
    (define-key map "," 'pop-to-mark-command)
    (define-key map "m" 'point-to-register)
    (define-key map "'" 'register-to-point)
    (define-key map "x" 'exchange-point-and-mark)
    (define-key map "w" 'clgrep-kill-ring-save)
    (define-key map "W" 'clgrep-copy-to-register)
    (substitute-key-definition
     'kill-ring-save 'clgrep-kill-ring-save map global-map)
    (substitute-key-definition
     'copy-to-register 'clgrep-copy-to-register map global-map)
    ;; misc
    (define-key map "\C-m" 'clgrep-goto-line)
    (setq clgrep-mode-map map)))


(defun clgrep-mode ()
  "Major mode for viewing output log from ChangeLog.

Keybindings:
\\{clgrep-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map clgrep-mode-map)
  (setq major-mode 'clgrep-mode
        mode-name "clgrep")
  (set (make-local-variable 'paragraph-start) "\\s *$\\|\f\\|^\\<")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'page-delimiter) "^\\<\\|^\f")
  (when (fboundp 'defface)
    (require 'add-log)
    (set (make-local-variable 'font-lock-defaults)
         `((,@change-log-font-lock-keywords ,@clgrep-font-lock-keywords)
           t nil nil backward-paragraph)))
  (run-hooks 'clgrep-mode-hook))

(defun clgrep-quit ()
  "Quit clgrep."
  (interactive)
  (quit-window))

(defun clgrep-quit-delete-window ()
  "Quit clgrep and delete window."
  (interactive)
  (if (one-window-p)
      (clgrep-quit)
    (delete-window)))


;; Move functions

(defun clgrep-forward-entry (arg)
  "Move forward to beggining of ChangeLog entry.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N entries."
  (interactive "p")
  (cond
   ((< arg 0) (forward-line -1))
   ((not arg) (setq arg 1)))
  (end-of-line)
  (re-search-forward "^\t\\* " nil t arg)
  (forward-line 0))

(defun clgrep-backward-entry (arg)
  "Move backward to beginning of ChangeLog entries.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move farward N entries."
  (interactive "p")
  (clgrep-forward-entry (- arg)))


;; Copy functions.

(defun clgrep-kill-ring-save (beg end)
  "Same as `kill-ring-save' but remove TAB in the beginning of line."
  (interactive "r")
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beg end)
      (goto-char (point-min))
      (while (re-search-forward "^\t" nil t)
        (replace-match ""))
      (kill-ring-save (point-min) (point-max)))))

(defun clgrep-copy-to-register (register beg end &optional delete-flag)
  "Same as `copy-to-register' but remove TAB in the beginning of line.
Actually, fourth optional argument DELETE-FLAG does nohting."
  (interactive "cCopy to register: \nr\nP")
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beg end)
      (goto-char (point-min))
      (while (re-search-forward "^\t" nil t)
        (replace-match ""))
      (copy-to-register register (point-min) (point-max) delete-flag))))


;; Goto line

(defun clgrep-goto-line ()
  "Go back to ChangeLog buffer."
  ;; Called only from clgrep mode.
  (interactive)
  (when (featurep 'clgrep)
    (let* ((file (clgrep-file-name))
	   (buf  (get-file-buffer file))
	   (pos  (point))
	   header entry)
      (unless clgrep-noquery
	(setq header (progn
		       (forward-line 1)
		       (re-search-backward (format "%s.+$" clgrep-header-regexp) nil t)
		       (match-string 0))
	      entry  (progn
		       (re-search-forward (format "^[\t]+\\*.+$"))
		       (match-string 0))))
      (if buf
	  (switch-to-buffer buf)
	(switch-to-buffer (find-file-noselect file)))
      (if clgrep-noquery
	  (goto-char pos)
	(goto-char (point-min))
	(search-forward header nil t)
	(search-forward entry  nil t)
	(forward-line 0)))))



(if (locate-library "clkwd")
    (require 'clkwd))
;;; clgrep.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
