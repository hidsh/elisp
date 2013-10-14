(cond (window-system (set-default-font "VL ゴシック-11")
                     (set-fontset-font
                      (frame-parameter nil 'font)
                      'japanese-jisx0208
                      '("VL ゴシック" . "unicode-bmp"))))

(prefer-coding-system 'utf-8)

(set-face-attribute 'default nil :family "menslo LG L" :height 140)
(set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "VL ゴシック") nil 'append) 
(set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0212 (font-spec :family "VL ゴシック") nil 'append) 
(add-to-list 'face-font-rescale-alist '("VL ゴシック" . 1.1))

;; for mozc (google 日本語入力)
(require 'ibus) 
(add-hook 'after-init-hook 'ibus-mode-on) 
(ibus-define-common-key ?\C-\s nil)
(ibus-define-common-key ?\C-/ nil) 
(setq ibus-cursor-color '("red" "blue" "limegreen"))

;X11とクリップボードを共有
(setq x-select-enable-clipboard t)
