;;
;; Filename: tags-file-create-recursive.el
;; Last modified: Fri Mar 03 2006 13:51:45 LMT
;; Author: gnrr
;;
;; description:
;;      指定のディレクトリ配下に、編集中のファイルに似合うTAGSファイルを
;;      再帰的に作成する
;;
;; requirements:
;;      find, xargs, (etags)
;;
;; usage:
;;      M-x tags-file-create-recursive
;;
;;
;; install:
;;      add into your dot.emacs.
;;
;;      (require 'tags-file-create-recursive)
;;
;;
;; customize:
;;       例えば拡張子 *.asm のアセンブリソースで使用する場合以下のようにする
;;
;;      (append  '(("^[^\\.]+[\\.]asm$" . "*.asm"))
;;               tags-file-create-recursive-lang-alist)
;;

(defvar tags-file-create-recursive-lang-alist
  '(("^[^\\.]+[\\.][HhCc]$" . "*.[CcHh]")       ; C言語プロジェクト
    ("^[^\\.]+[\\.]cpp$" . "*.cpp")             ; C++プロジェクト
    ("^.+[\\.]el$" . "*.el"))                   ; elispプロジェクト
  "TAGSファイル自動作成用判別ALIST (ソースファイル名判別用 . find用)")

(defun process-each-sub-directories (DIR PROC &optional DOT-PAIR)
  "指定したディレクトリ DIR 以下に対し、ディレクトリ毎に１回づつ PROC を実行す
る汎用ディレクトリ走査関数。PATTERN がnilの場合は無条件にディレクトリ毎に１回
づつ PROC が実行されるが、nil以外の場合は PATTERN の car に合致したファイル名
がそのディレクトリに含まれる場合のみ実行される。"
  (let ((files (directory-files DIR))
        (processed nil)                 ; nil: 処理未実行
        (dir (directory-file-name DIR))
        filename path type)
    (while files
      (setq filename (car files))
      (setq path (concat dir "/" filename))
      (if (and (file-directory-p path)
               (not (string-match "^[\.]+$" filename))) ; "." と ".." はディレクトリとしてではなくファイルとして扱う
          ; ディレクトリ
          (process-each-sub-directories path PROC DOT-PAIR) ; 再帰
        ; ファイル
        (unless processed               ; PROC未実行の場合
          (when (or (not DOT-PAIR)      ; ファイル名が指定がない もしくは
                    (and DOT-PAIR       ; ファイル名指定があって かつ そのファイルを発見した場合のみ
                         (string-match (car DOT-PAIR) filename)))
            (funcall PROC dir (cdr DOT-PAIR))           ; ディレクトリ毎の処理を実行
            (setq processed t))))       ; t: 既実行
      (setq files (cdr files)))))


(defun process-each-tags-file-create (dir pattern)
  "ディレクトリごとのTAGSファイル作成関数"
  (let* ((rel (directory-file-name (file-relative-name top-dir)))
         ; top-dir はcreate-tags-file-recursiveで束縛されたものを参照
         (cmd (concat "cd " (expand-file-name dir)
                      "; find " rel " -type f -name \"" pattern
                      "\" -print | xargs etags")))
         ; サブディレクトリに一旦下りて、そこから top-dir への上向き相対パス
         ; から find する
    (shell-command cmd "aaaa")))


(defun tags-file-create-recursive (top-dir)
  "指定のディレクトリ配下に、編集中のファイルに似合うTAGSファイルを再帰的に
作成する"
  (interactive "Dtop dir: ")
  (let ((lang tags-file-create-recursive-lang-alist)
         (filename (file-name-nondirectory (buffer-file-name))))
    (while lang
      (when (string-match (caar lang) filename)
        ; カレントバッファのファイルが何らかのソースファイルと判別できた
        ; 場合のみTAGSファイルを作成する
        (message "creating TAGS files now...")
        (process-each-sub-directories top-dir
                                      'process-each-tags-file-create
                                      (car lang))
        (message "created TAGS files.")
        (setq lang ()))
      (setq lang (cdr lang)))))


;; for autoload
(provide 'tags-file-create-recursive)

;; tags-file-create-recursive.el ends here
