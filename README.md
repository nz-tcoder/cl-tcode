# cl-tcode
lemで動くTコードmode。GNU Emacs用のtc2、ベースしたソースは melpa版(tc-20150113.1926)、をcommon lispに移植したものです。

Tコードについては、http://openlab.jp/tcode/ を参照ください。

## 機能
* tコードのみ。tutコードなど他の漢直コードを対応していない。
* 混ぜ書き変換実装。
* 練習プログラム(lemminggg)。EELLLTXT.md参照。

## 未実装
* 各種ヘルプ機能。
* 部首変換。
* 混ぜ書き用辞書への単語登録。
* isearchでの入力。


## 設定
使ってみる。

* ros install nz-tcoder/cl-tcode
* M-x lisp-switch-to-repl-buffer
* CL-USER> (ql:quickload "lem-tcode")
* M-\でtcode-modeをトグルします。

普段から使う場合はさらに、以下でlemのdumpを作り直す。

* M-x site-init-add-dependency [RET] tcode [RET]
* ros install lem


