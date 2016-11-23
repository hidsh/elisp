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

;;
;; font
;;
;; (set-face-attribute 'default nil :family "menlo" :height 140) 
;; (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "hiragino maru gothic pron") nil 'append) 
;; (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0212 (font-spec :family "hiragino maru gothic pron") nil 'append) 
;; (add-to-list 'face-font-rescale-alist '("^-apple-hiragino_.*" . 1.1))

;;
;; tabbar
;;
(require 'tabbar+)
(tabbar-mode 1)

;; タブ上でマウスホイール操作無効
(tabbar-mwheel-mode -1)

;; グループ化しない
(setq tabbar-buffer-groups-function nil)

;; 左に表示されるボタンを無効化
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))


;; タブに表示させるバッファの設定
(defvar my-tabbar-displayed-buffers
  '("*scratch*" "*Moccur*" "*eshell*")
  "*Regexps matches buffer names always included tabs.")

(defun my-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my-tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt my-tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))

(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
;; (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

(global-set-key [f12]        'tabbar-forward-tab)
(global-set-key [f11]        'tabbar-backward-tab)

(global-set-key [(meta f12)] 'tabbar+move-right)
(global-set-key [(meta f11)] 'tabbar+move-left)

(defun my-tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (with-current-buffer buffer
        (kill-buffer)))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)))

;(setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
(setq tabbar-select-tab-function 'my-tabbar-buffer-select-tab)

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format " [%s] " (tabbar-tab-tabset tab))
                  (format " %s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; 外観変更
(set-face-attribute 'tabbar-default nil
 :family "Lucida Grande" :height 130
 :background "#bebdbe")


(set-face-attribute 'tabbar-unselected nil
 :background "#bebdbe"
 :foreground "#313131"
 :box nil)

(set-face-attribute 'tabbar-selected nil
 :background "#22232a"
 :foreground "#e9e9e9"
 :box nil)

(setq tabbar-separator '(0.2)) ;; タブの長さ
(set-face-attribute 'tabbar-separator nil
 :background "#6c6c6c")


;;;
;;; open terminal
;;;
(defun open-terminal-1 (dir)
  (let* ((term "iTerm 2")
         (script (concat
                  "if application \"" term "\" is running then\n"
                  "  tell application \"" term "\"\n"
                  "    set _window to (create window with profile \"Default\")\n"
                  "    tell _window\n"
                  "      tell current session\n"
                  "        write text \"cd " dir "; clear\"\n"
                  "      end tell\n"
                  "    end tell\n"
                  "  end tell\n"
                  "else\n"
                  "  tell application \"" term "\"\n"
                  "    activate\n"
                  "    tell current window\n"
                  "      tell current session\n"
                  "        write text \"cd " dir "; clear\"\n"
                  "      end tell\n"
                  "    end tell\n"
                  "  end tell\n"
                  "end if\n")))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))
    ;; (insert script)))

;; (defun open-terminal-1 (dir)
;;   (let* ((term "Terminal")
;;          (script (format (concat
;;                          "tell application \"" term "\"\n"
;;                          "    activate\n"
;;                          "    do script with command \" cd %s \" \n"
;;                          "end tell\n")
;;                         ;; (concat "\\\"" dir "\\\""))))
;;                         dir)))
;;     (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(defun open-terminal ()
  (interactive)
  (let* ((fn (buffer-file-name))
         (dir (cond ((string= major-mode "dired-mode") dired-directory)
                    ((and fn (file-exists-p fn)) (file-name-directory fn))
                    (t "~"))))
    (open-terminal-1 dir)))

(setf term-orig (symbol-function 'term))
(fset 'term 'open-terminal)
(fset 'c 'open-terminal)


;;; 
;;; open finder
;;; 
(defun open-finder-1 (dir fn)
  (let ((script
		 (if fn
             (let ((path (concat dir fn)))
               (concat
                "tell application \"Finder\"\n"
                "    set frontmost to true\n"
                "    make new Finder window to (POSIX file \"" dir "\")\n" 
                ;; "    select file \"" file "\"\n"
                "    select (\"" path "\" as POSIX file)\n"
                "end tell\n"))
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
