;;; mac.el --- Mac specifics

;; Copyright (C) 2013  g

;; Author: g <g@mac-mini.local>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;
;;; open terminal
;;;
(defun open-terminal-1 (dir)
  (let ((script (format (concat
                         "tell application \"Terminal\"\n"
                         "    activate"
                         "    do script with command \"cd %s\"\n"
                         "end tell\n")
                        dir)))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(defun open-terminal ()
  (interactive)
  (let* ((fn (buffer-file-name))
         (dir (cond ((string= major-mode "dired-mode") dired-directory)
                    ((and fn (file-exists-p fn)) (file-name-directory fn))
                    (t "~"))))
    (open-terminal-1 dir)))

(setf term-orig (symbol-function 'term))
(fset 'term 'open-terminal)


;;; 
;;; open finder
;;; 
(defun open-finder-1 (dir file)
  (let ((script
		 (if file
			 (concat
			  "tell application \"Finder\"\n"
			  "    set frontmost to true\n"
			  "    make new Finder window to (POSIX file \"" dir "\")\n" 
			  "    select file \"" file "\"\n"
			  "end tell\n")
		   (concat
			"tell application \"Finder\"\n"
			"    set frontmost to true\n"
			"    make new Finder window to {path to desktop folder}\n"
			"end tell\n"))))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(defun open-finder ()
  (interactive)
  (let ((path (buffer-file-name))
		dir file)
	(when path
	  (setq dir (file-name-directory path))
	  (setq file (file-name-nondirectory path)))
	(open-finder-1 dir file)))
  
(defalias 'e 'open-finder)

;;;
;;; open by default app
;;;
(defun open-browser ()
  (interactive)
  (let ((browser "\"/Applications/Google Chrome.app\""))
    (shell-command (concat "open -a " browser " \"" (buffer-file-name) "\""))))


(provide 'mac)
;;; mac.el ends here
