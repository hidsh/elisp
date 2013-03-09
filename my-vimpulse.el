(setq viper-mode t)
(require 'vimpulse)

;; Vimpulse終了/起動
(define-key global-map [(A-kp-delete)] 'toggle-viper-mode)

;; 物理行移動を基本にする
(vimpulse-map "j" 'next-line)
(vimpulse-map "k" 'previous-line)
(vimpulse-map [?\C-n] 'viper-next-line)
(vimpulse-map [?\C-p] 'viper-previous-line)

;; h l は行頭/行末を超えられるようにする
(vimpulse-map "h" 'backward-char)
(vimpulse-map "l" 'forward-char)

;; deleteと backspaceキーは文字削除
(vimpulse-map [delete] 'vimpulse-delete)
(vimpulse-map [backspace] 'backward-delete-char-untabify)

;; Spaceキーで画面スクロール
(vimpulse-map [?\ ] 'viper-scroll-screen)
;; Shift+Spaceキーで画面逆スクロール
(vimpulse-map [?\S-\ ] 'viper-scroll-screen-back)
(vimpulse-map "b" 'viper-scroll-screen-back)


(vimpulse-map "4" 'end-of-line)
;(vimpulse-map "H" 'viper-backward-word)
;; (vimpulse-map "L" 'viper-forward-word)
(vimpulse-map ";" 'viper-ex)
;; (vimpulse-map ";" 'commentize-and-next-line)

(vimpulse-map [?\C-s] 'vimpulse-search-forward)
(vimpulse-map [?\C-l] 'recenter)
(vimpulse-map [?\C-o] 'other-window-or-split)

(provide 'my-vimpulse)
