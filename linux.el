;;
;; provisional
;;
(defalias 'scroll-up-command 'scroll-up)     ; for emacs23
(defalias 'scroll-down-command 'scroll-down) ; for emacs23


;;
;; font
;;
(cond (window-system (set-default-font "VL ゴシック-11")
                     (set-fontset-font
                      (frame-parameter nil 'font)
                      'japanese-jisx0208
                      '("VL ゴシック" . "unicode-bmp"))))

(prefer-coding-system 'utf-8)

;; (set-face-attribute 'default nil :family "Meslo LG S" :height 120)
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 140)
;; (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "VL ゴシック") nil 'append) 
;; (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0212 (font-spec :family "VL ゴシック") nil 'append) 
(add-to-list 'face-font-rescale-alist '("VL ゴシック" . 1.1))

;;
;; for mozc (google 日本語入力)
;;
(require 'ibus) 
(add-hook 'after-init-hook 'ibus-mode-on) 
(ibus-define-common-key ?\C-\s nil)
(ibus-define-common-key ?\C-/ nil) 
(setq ibus-cursor-color '("red" "blue" "limegreen"))

;;
;; X11とクリップボードを共有
;;
(setq x-select-enable-clipboard t)

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

(setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
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
 :family "Lucida Grande" :height 100
 :background "#bebdbe")


(set-face-attribute 'tabbar-unselected nil
 :background "#bebdbe"
 :foreground "#313131"
 :box nil)

(set-face-attribute 'tabbar-selected nil
 :background "#22232a"
 :foreground "#e9e9e9"
 :box nil)

(setq tabbar-separator '(0.1)) ;; タブの長さ
(set-face-attribute 'tabbar-separator nil
 :background "#6c6c6c")
