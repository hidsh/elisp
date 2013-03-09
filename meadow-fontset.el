;; meadow-fontset.el

;;; font-lockの設定
;(if window-system
;    (progn
;      (setq font-lock-support-mode 'lazy-lock-mode)
;      (global-font-lock-mode t)))
(global-font-lock-mode t)

;; ;;; TrueType フォント設定
(w32-add-font
 "tt-font"
 '((strict-spec
    ((:char-spec ascii :height any)
     (w32-logfont "Andale Mono" 8 16 400 0 nil nil nil 0 1 3 49))
    ((:char-spec ascii :height any :weight bold)
     (w32-logfont "Andale Mono" 8 16 700 0 nil nil nil 0 1 3 49))
    ((:char-spec ascii :height any :slant italic)
     (w32-logfont "Andale Mono" 8 16 400 0   t nil nil 0 1 3 49))
    ((:char-spec ascii :height any :weight bold :slant italic)
     (w32-logfont "Andale Mono" 8 16 700 0   t nil nil 0 1 3 49))
    ((:char-spec japanese-jisx0208 :height any)
     (w32-logfont "ＭＳ ゴシック" 0 16 400 0 nil nil nil 128 1 3 49))
    ((:char-spec japanese-jisx0208 :height any :weight bold)
     (w32-logfont "ＭＳ ゴシック" 0 16 700 0 nil nil nil 128 1 3 49)
     ((spacing . -1)))
    ((:char-spec japanese-jisx0208 :height any :slant italic)
     (w32-logfont "ＭＳ ゴシック" 0 16 400 0   t nil nil 128 1 3 49))
    ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
     (w32-logfont "ＭＳ ゴシック" 0 16 700 0   t nil nil 128 1 3 49)
     ((spacing . -1))))))

(set-face-attribute 'variable-pitch nil :font "tt-font")

;; フレームフォントの設定
(setq default-frame-alist
      (cons '(font . "tt-font")
	    default-frame-alist))

;; ;;; BDF フォント設定
;; ;; intlfonts-1.2.tar.gz が必要です
;; ;;	http://www.ring.gr.jp/archives/GNU/intlfonts/intlfonts-1.2.tar.gz
;; ;;	http://ring.asahi-net.or.jp/archives/GNU/intlfonts/intlfonts-1.2.tar.gz
;; ;;	ftp://ftp.m17n.org/pub/mule/intlfonts-1.2.tar.gz
;; ;;	ftp://ftp.gnu.org/pub/gnu/intlfonts/intlfonts-1.2.tar.gz
;; ;; などから入手してください
;; ;; c:/MEADOW/intlfonts-1.2 と展開した場合の設定例となっています。
;; (defvar bdf-font-directory "c:/MEADOW/intlfonts-1.2")
;; (defvar bdf-font-name-prefix "bdffont16-")
;;
;; (defvar bdf-font-file-alist
;;   '((ascii "European/lt1-16-etl.bdf" 0)
;;     (latin-iso8859-1 "European/lt1-16-etl.bdf" 1)   ; ISO8859-1
;;     (latin-iso8859-2 "European/lt2-16-etl.bdf" 1)   ; ISO8859-2
;;     (latin-iso8859-3 "European/lt3-16-etl.bdf" 1)   ; ISO8859-3
;;     (latin-iso8859-4 "European/lt4-16-etl.bdf" 1)   ; ISO8859-4
;;     (cyrillic-iso8859-5 "European/cyr16-etl.bdf" 1) ; ISO8859-5
;;     (arabic-iso8859-6 "Misc/arab16-0-etl.bdf" 0)    ; ISO8859-6 ???
;;     (greek-iso8859-7 "European/grk16-etl.bdf" 1)    ; ISO8859-7
;;     (hebrew-iso8859-8 "Misc/heb16-etl.bdf" 1)       ; ISO8859-8
;;     (latin-iso8859-9 "European/lt5-16-etl.bdf" 1)   ; ISO8859-9
;;     (thai-tis620 "Asian/thai16.bdf" 1)	       ; TIS620
;;     (katakana-jisx0201 "japanese.X/8x16rk.bdf" 1)   ; JISX0201
;;     (latin-jisx0201 "japanese.X/8x16rk.bdf" 0)      ; JISX0201
;;     (japanese-jisx0212 "japanese/jksp16.bdf" 0)     ; JISX0212
;;     (japanese-jisx0208-1978 "japanese/j78-16.bdf" 0); JISX0208.1978
;;     (japanese-jisx0208 "japanese.X/jiskan16.bdf" 0) ; JISX0208.1983
;; ;;	 (japanese-jisx0213-1 "Japanese.X/jiskan16-2000-1.bdf" 0)   ; JISX0213-2000(Plane 1) * Mule-UCS が必要です
;; ;;	 (japanese-jisx0213-2 "Japanese.X/jiskan16-2000-2.bdf" 0)   ; JISX0213-2000(Plane 2) * Mule-UCS が必要です
;;     (korean-ksc5601 "Korean.X/hanglm16.bdf" 0)      ; KSC5601   mincho
;; ;;	 (korean-ksc5601 "Korean.X/hanglg16.bdf" 0)    ; KSC5601   gothic
;;     (chinese-gb2312 "Chinese.X/gb16fs.bdf" 0 )      ; GB2312	 ???
;;     (chinese-cns11643-1 "Chinese/cns1-16.bdf" 0)    ; CNS11643.1992-1
;;     (chinese-cns11643-2 "Chinese/cns2-16.bdf" 0)    ; CNS11643.1992-2
;;     (chinese-cns11643-3 "Chinese/cns3-16.bdf" 0)    ; CNS11643.1992-3
;;     (chinese-cns11643-4 "Chinese/cns4-16.bdf" 0)    ; CNS11643.1992-4
;;     (chinese-cns11643-5 "Chinese/cns5-16.bdf" 0)    ; CNS11643.1992-5
;;     (chinese-cns11643-6 "Chinese/cns6-16.bdf" 0)    ; CNS11643.1992-6
;;     (chinese-cns11643-7 "Chinese/cns7-16.bdf" 0)    ; CNS11643.1992-7
;;     (chinese-big5-1 "Chinese/taipei16.bdf" encode-big5-font) ; Big5
;;     (chinese-big5-2 "Chinese/taipei16.bdf" encode-big5-font) ; Big5
;;     (chinese-sisheng "Chinese/sish16-etl.bdf" 0)    ; sisheng_cwnn ???
;;     (vietnamese-viscii-lower "Asian/visc16-etl.bdf" encode-viscii-font) ; VISCII1.1
;;     (vietnamese-viscii-upper "Asian/visc16-etl.bdf" encode-viscii-font) ; VISCII1.1
;;     (arabic-digit "Misc/arab16-0-etl.bdf" 0)	       ; MuleArabic-0
;;     (arabic-1-column "Misc/arab16-1-etl.bdf" 0)     ; MuleArabic-1
;;     (arabic-2-column "Misc/arab16-2-etl.bdf" 0)     ; MuleArabic-2
;;     (ipa "Misc/ipa16-etl.bdf" 1)		       ; MuleIPA
;;     (ethiopic "Ethiopic/ethio16f-uni.bdf" encode-ethio-font) ; Ethiopic-Unicode
;; ;;	 (ascii-right-to-left "European/lt1-16-etl.bdf" 0) ; ISO8859-1 ;; ???
;;     (indian-is13194 "Asian/isci16-mule.bdf" 0)      ; IS13194-Devanagari
;;     (indian-2-column "Asian/ind16-mule.bdf" 0)      ; MuleIndian-2
;;     (indian-1-column "Asian/ind1c16-mule.bdf" 0)    ; MuleIndian-1
;;     (lao "Asian/lao16-mule.bdf" 1)		       ; MuleLao-1
;;     (tibetan "Asian/tib16-mule.bdf" 0)	       ; MuleTibetan-0
;;     (tibetan-1-column "Asian/tib1c16-mule.bdf" 0)   ; MuleTibetan-1
;;     ))
;;
;; (defun w32-configure-bdf-font (fontset)
;;   (new-fontset fontset
;;	       (mapcar
;;		(lambda (x)
;;		  (let* ((charset (car x))
;;			 (filename (nth 1 x))
;;			 (encoding (nth 2 x))
;;			 (fontname
;;			  (concat bdf-font-name-prefix
;;				  (symbol-name charset))))
;;		    (w32-auto-regist-bdf-font
;;		     fontname
;;		     (expand-file-name filename bdf-font-directory)
;;		     encoding)
;;		    (cons charset fontname)))
;;		bdf-font-file-alist)))
;;
;; (w32-configure-bdf-font "bdf-fontset")
;;
;; ;; bold, italic, bold itaric を追ﾁ。
;; ;; Bold
;; (w32-change-font-logfont "bdffont16-ascii" 1
;;     (list 'bdf-font (expand-file-name "European/lt1-16b-etl.bdf" bdf-font-directory )))
;; ;; italic
;; (w32-change-font-logfont "bdffont16-ascii" 2
;;     (list 'bdf-font (expand-file-name "European/lt1-16i-etl.bdf" bdf-font-directory )))
;; ;; Bold itaric
;; (w32-change-font-logfont "bdffont16-ascii" 3
;;     (list 'bdf-font (expand-file-name "European/lt1-16bi-etl.bdf" bdf-font-directory )))


;; 初zフレームの設定
(setq default-frame-alist
      (append (list '(foreground-color . "black")
;;		    '(background-color . "LemonChiffon")
;;		    '(background-color . "gray")
		    '(background-color . "cornsilk2")
		    '(border-color . "black")
		    '(mouse-color . "white")
		    '(cursor-color . "khaki4")
		    '(ime-font . "Nihongo-12") ; TrueType のみ
;;		    '(font . "bdf-fontset")    ; BDF
;;		    '(font . "private-fontset"); TrueType
		    '(width . 80)
		    '(height . 40) ;; default
		    '(top . 48)   ;; thinkpad x20
		    '(left . 338)  ;; thinkpad x20
		    )
	      default-frame-alist))

;; IME用フォント設定
(setq default-frame-alist
      (cons '(ime-font . (w32-logfont "ＭＳ ゴシック"
				      0 16 400 0 nil nil nil 128 1 3 17))
	    default-frame-alist))


;; タブ, 全角スペース, 行末スペースを表示する
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=visual%20tab
;;(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "white"))) nil) ;; 全角スペース
(defface my-face-b-2 '((t (:background "beige"))) nil) ;; TAB
(defface my-face-u-1 '((t (:foreground "ivory3" :underline t))) nil)
;;(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)	;; TAB
     ("　" 0 my-face-b-1 append)	;; 全角スペース
     ("[ \t]+$" 0 my-face-u-1 append)	;; 行末のスペース
     ;;("[\r]*\n" 0 my-face-r-1 append)	;;�行
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
