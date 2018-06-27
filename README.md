# cl-tcode
lemで動くTコードmode。GNU Emacs用のtc2、ベースしたソースは melpa版(tc-20150113.1926)、をcommon lispに移植したものです。

Tコードについては、http://openlab.jp/tcode/ を参照ください。

## 機能
* tコードのみ。tutコードなど他の漢直コードを対応していない。
* 混ぜ書き変換実装。

## 未実装
* 各種ヘルプ機能。
* 部首変換。
* 混ぜ書き用辞書への単語登録。
* isearchでの入力。
* 練習プログラム。

## 設定
* ros install nz-tcoder/cl-tcode
* M-x site-init-add-dependency [RET] lem-tcode [RET]
* ros install lemでlemのdumpを作り直す。
* `C-\`でtcode-modeをトグルします。
