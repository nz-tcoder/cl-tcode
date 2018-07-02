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
* cl-tcodeをasdfのライブラリとしてロードできるようにする。
* 混ぜ書き用辞書(mazegaki.dic)を`*tcode-dir*`(任意で可。ex. ~/cl-tcode)に置く。
* tcテーブル(tc-tbl.lisp)を`*tcode-dir*`に置く。
* setup-tcodeの引数に指定したキーでtcode-modeをトグルします。

### 混ぜ書き用辞書
* tc2のmazegaki.dicであればOK。ただし、文字コードをutf-8にしてください。
* mazegaki.dicはTコードページからダウンロードできるtc2から作成することができます。
* Tコードページ以外にもtc2は公開されています。 https://github.com/kozo2/tc (melpa版のソース?)では mazegaki.dic が同梱されています。

### .lemrc例
```
(asdf:initialize-source-registry
 '(:source-registry
   (:tree (:home "git/cl-tcode"))
   :inherit-configuration))

(require :cl-tcode)

(in-package :lem.tc-mode)
(setq *tcode-dir* "cl-tcode")

(setup-tcode (merge-pathnames (format nil "~a/~a" *tcode-dir* "tc-tbl.lisp")
                              (user-homedir-pathname)))

(setup-mazegaki-dic (merge-pathnames (format nil "~a/~a" 
                                             *tcode-dir*
                                             *tcode-dic-file-name*)
                                     (user-homedir-pathname))
             "C-\\")

```

