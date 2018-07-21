(defpackage :lem.tc-mode
  (:use :cl :lem)
  (:export :tc-mode :wj))

(in-package :lem.tc-mode)

(defvar *tc-engine* nil)

(defgeneric tcode-decode-verbose (engine)
  (:documentation "文字入力の途中で、今までの入力をすべてそのまま挿入する。
                 another-tableがnilでなければ、入力に応じた文字を入力する。"))

(defgeneric tcode-decode (engine)
  (:documentation "入力ストロークから文字に変換する。
                   Return (values status value) where:
                      STATUS             VALUE
                      complete           decoded value
                      incomplete         nil"))

(defun setup-tcode-table (size base-table non-2-stroke)
  (loop repeat size
        with new-table = (make-array (list size size))
        for k1 from 0 
        for v in base-table
        for newval = (if v (make-array size
                                       :initial-contents (remove #\space v)))
        do
           (if (and v (not (= (length newval) size)))
               (error "Table corrupted at line ~d." (1+ k1)))
           (loop for char across newval
                 for k2 from 0
                 do
                    (unless (find char non-2-stroke)
                      (setf (aref new-table k1 k2) char)))
        finally
           (return new-table)))

(defvar *tcode-clear-hook* nil)

(defvar *tcode-cancel-stroke-list* '(#\backspace #\rubout)
  "文字入力を明示的に取り消すキーのリスト")

(defvar *tcode-verbose-stroke-list* '(#\space)
  "文字入力の途中で、今までの入力をすべてそのまま挿入するキーのリスト")

(defconstant +ASCII-SPACE+ (char-code #\space))
(defconstant +ASCII-TILDE+ (char-code #\~))
(defconstant +ASCII-MAX+ 127)

(defvar *zen-han-map* (make-hash-table)
    "半角英数字と全角英数字変換用ハッシュ表。")

(defvar *tcode-stroke-buffer-name* " *tcode: stroke*")
(defvar *tcode-help-buffer-name* "*T-Code Help*")

(defvar *tcode-char-list*
  (append (loop for code from (char-code #\a) to (char-code #\z)
                collect (code-char code))
          (loop for code from (char-code #\0) to (char-code #\9)
                collect (code-char code))
          '(#\; #\, #\. #\/)))

(defvar *tcode-verbose-message* t
  "* non-nil のとき、より多くのメッセージを表示する。"
)

;; for overlay
(define-attribute conversion
  (t :underline-p t))

(define-attribute inflection
  (t :reverse-p t))

(defun hankaku-p (ch)
  (let ((code (char-code ch)))
    (and (>= code +ASCII-SPACE+)
         (>= +ASCII-MAX+ code))))

(defun setup-zen-han-map ()
  (let ((zenkaku  ; 半角にした場合のアスキーコード順
          (format nil "~@{~a~}"
                  "　！”＃＄％＆’（）＊＋，−．／０１２３４５６７８９：；＜＝＞？"
                  "＠ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ［¥］＾＿"
                  "‘ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ｛｜｝‾")))
    (loop for z across zenkaku
          for code from +ASCII-SPACE+
          for ch = (code-char code)
          do
             (setf (gethash z *zen-han-map*) ch
                   (gethash ch *zen-han-map*) z))))

(defun to-zenkaku (ch)
  (alexandria:if-let ((zenkaku (gethash ch *zen-han-map*)))
    zenkaku
    ch))

(defun tcode-verbose-message (message &optional non-verbose-message)
  "変数 `*tcode-verbose-message*' が non-nil の場合には、 MESSAGE を表示する。
そうでないとき、 NON-VERBOSE-MESSAGE があれば、これを表示する。"
  (and (or *tcode-verbose-message* non-verbose-message)
       (message (if *tcode-verbose-message* message non-verbose-message))))

(defun tcode-display-help-buffer (content)
  "\"*T-Code Help*\" というバッファに content の内容を表示する。"
  (with-pop-up-typeout-window (s (make-buffer *tcode-help-buffer-name*
                                              :read-only-p t)
                                 :erase t)
    (format s "~a" content)))

(defun tcode-remove-help-buffer ()
  (alexandria:if-let (win (car (get-buffer-windows
                                (get-buffer *tcode-help-buffer-name*))))
    (delete-window win)))

(define-command tcode-clear () ()
  "ややこしいモードに入っているのを全部クリアする。
ヘルプ用ウィンドウも消去する。"
  (tcode-remove-help-buffer)
  (run-hooks *tcode-clear-hook*))

(defun make-table-format (width-list)
    (loop for width in width-list
          for i from 0
          collect (format nil "~~a~~~d~c/lem.tc-mode:wj/" width
                          (if (>= i 5) #\: #\@))))

(defun make-column-width (table none-str)
  (loop for i from 0 to 9
        collect (apply 'max 4 (mapcar #'(lambda (x)
                                          (string-width
                                           (or (aref table (+ x i)) none-str)))
                                      '(0 10 20 30)))))
                                        
(defun tcode-draw-table (candidate-table page whole-page)
  (let* ((sep0 '(" " " " " " " " "  " "  " "  " " " " " " " ""))
         (sep1 '("[" " " " " " " "] " "  " " [" " " " " " " "]"))
         (none-str "-")
         (fmt-list (make-table-format (make-column-width candidate-table
                                                         none-str))))
    (with-output-to-string (s)
      (labels ((output-row (separator row)
                 (with-output-to-string (st)
                   (loop for col from 0 to 9
                         do
                            (format st (nth col fmt-list) (nth col separator)
                                    (or (aref candidate-table (+ (* row 10)
                                                                 col))
                                        none-str)))
                   (format st "~a" (nth 10 separator)))))
        (format s "~a~%~{~a~^~%~}"
                (output-row sep0 0)
                (loop for row from 1 to 3
                      collect (output-row sep1 row)))
        (if (> whole-page 1)
          (format s "     (~d/~d)" page whole-page))
        (format s "~%")))))

(define-minor-mode tc-mode
    (:name "tc"
     :keymap *tc-mode-keymap*)
  (tcode-clear))

(defun tc-mode-p ()
  (mode-active-p (current-buffer) 'tc-mode))

(defun tcode-set-action-to-table (strokes value table)
  "コード入力用の内部テーブルに入力列 STROKES に対する VALUE を設定する。
動作(VALUE)として指定できるのは以下のとおり。

  ただし、cl-tcodeでは、以下の中でコマンドとリスト(ただし関数名と引数と解釈)
  のみ対応。

    - コマンド (symbol)そのコマンドを実行する。
    - 関数 (symbol, lambda式)その関数を引数なしで呼ぶ。
    - 変数 (symbol)評価した結果の動作を行う。
    - 表 (vector)更にその表に従った動作を行う。
    - リスト (list)更にそのリストに従った動作を行う。
    - 文字列 (string)その文字列を挿入する。
    - 文字 (char)その文字を挿入する。

  入力列はキーの番地のリストまたはキーの番地。
キーの番地を指定すると、最後に SPC を押したときの動作を設定する。"
  (cond ((consp strokes)
         (setf (aref table (first strokes) (second strokes)) value))
        (t
          (error "入力列の指定が無効です。"))))

(defclass tc-engine ()
  ((table
    :initarg :table
    :reader table
    :documentation "漢直変換用のストロークと漢字の対応を示すテーブル")
   (table-size
    :initarg :table-size
    :reader table-size
    :initform nil
    :documentation "漢直変換用のストロークと漢字の対応を示すテーブルのサイズ")
   (base-table
    :initarg :base-table
    :documentation "tableの元となるリスト")
   (another-table
    :initarg :another-table
    :initform nil
    :reader another-table
    :documentation  "拡張用テーブル")
   (non-2-stroke-char-list
    :initarg :non-2-stroke
    :initform nil
    :reader non-2-stroke-char-list
    :documentation  "base-tableでspecial-commandにアサインされている場所を指定")
   (special-command-alist
    :initarg :special-command-alist
    :initform nil
    :reader special-command-alist
    :documentation  "混ぜ書き変換うヘルプ表示などに使うストロークとコマンド")
   (strokes
    :initform nil
    :reader strokes
    :documentation "それまでに入力したスロトーク ((char . code) ...)")
   (use-hankaku
    :initform t
    :documentation "英数字を半角で表示する(ディフォルト)")
   (help-string
    :initarg :help-string
    :initform nil
    :reader help-string
    :documentation "message for tcode-mode-help")
   (key-translation-rule-table
    :initform
     ;;   0  1  2  3  4  5  6  7  8  9
     ;;  10 11 12 13 14 15 16 17 18 19
     ;;  20 21 22 23 24 25 26 27 28 29
     ;;  30 31 32 33 34 35 36 37 38 39
     
     ;;   1  2  3  4  5  6  7  8  9  0
     ;;   q  w  e  r  t  y  u  i  o  p
     ;;   a  s  d  f  g  h  j  k  l  ;
     ;;   z  x  c  v  b  n  m  ,  .  /
  
     ;;      !  \"   #   $   %   &   '   (   )   *   +   ,   -   .   /
     ;;  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
     ;;  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
     ;;  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
     ;;  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
     ;;  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
    (make-array
     95
     :initial-contents
     '(-1 -3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 37 -1 38 39
       09 00 01 02 03 04 05 06 07 08 -1 29 -1 -3 -1 -3
       -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
       -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
       -1 20 34 32 22 12 23 24 25 17 26 27 28 36 35 18
       19 10 13 21 14 16 33 11 31 15 30 -1 -3 -1 -1))
     :reader key-translation-rule
     :documentation
  "Tコードキー変換用テーブル。その1文字を入力したときの意味を表す。
0..39:	Tコードキー。
-1:	その文字。
-2:	対応する英小文字。
-3:	`tcode-mode-map' にしたがったコマンド。
< -3:	- (文字コード)。")))

(defun tcode-char-to-key (engine c)
  "Return virtual key code of character C."
  (let ((code (char-code c)))
    (if (or (< code +ASCII-SPACE+) (> code +ASCII-TILDE+))
        -1
        (aref (key-translation-rule engine)
              (- code +ASCII-SPACE+)))))

(defmethod initialize-instance :after ((engine tc-engine) &key)
  (with-slots (table table-size base-table non-2-stroke-char-list special-command-alist) engine
    (setf table
          (setup-tcode-table table-size base-table non-2-stroke-char-list))
    (loop for (strokes . value) in special-command-alist
          do
             (tcode-set-action-to-table (reverse strokes) value table))))

(defmethod toggle-alnum ((engine tc-engine))
  (with-slots (use-hankaku) engine
    (setf use-hankaku (not use-hankaku))))

(defmethod filter ((engine tc-engine) ch)
  (with-slots (use-hankaku) engine
    (if (and (not use-hankaku) (hankaku-p ch))
        (gethash ch *zen-han-map*)
        ch)))

(defmethod tcode-decode-verbose ((engine tc-engine))
  (concatenate 'string (nreverse (mapcar #'car (strokes engine)))))

(defmethod push-input-char ((engine tc-engine) ch)
  "chに対応するkeyを返す。"
  (let ((key (tcode-char-to-key engine ch)))
    (with-slots (strokes) engine
      (push (cons ch key) strokes))
    key))

(defmethod clear-strokes ((engine tc-engine))
  (with-slots (strokes) engine
    (setf strokes nil)))

(defmethod tcode-decode ((engine tc-engine))
  (with-slots (strokes table) engine
    (if (< (length strokes) 2)
        (list 'incomplete nil nil)
        (list 'complete
              (aref table (cdr (first strokes)) (cdr (second strokes)))
              (mapcar #'car strokes)))))

(defun tcode-not-key-action (engine ch key)
  (let ((trace (mapcar #'car (strokes engine))))
    (cons (cond ((and trace
                      (find ch *tcode-cancel-stroke-list*))
                 t)
                ((and trace
                      (find ch *tcode-verbose-stroke-list*))
                 (tcode-decode-verbose engine))
                ((= key -1)
                 ch)
                ((= key -2)
                 (message "key is -2, ch is ~c" ch)
                 (char-downcase ch))
                (t
                 (- key)))
          (nreverse (cons ch trace)))))

(defun tcode-decode-chars (engine ch)
  "Return value: cons of (decoded value . character sequence)
   where decoded value is one of:
    char or string or function (symbol or lambda) ... code
    nil ... no corresponding code
    t ... cancel"
  (let ((key (tcode-char-to-key engine ch)))
    (cond ((minusp key)
           ;; ch is not a key on TABLE
           (let ((result (tcode-not-key-action engine ch key)))
             (clear-strokes engine)
             result))
          (t
           ;; ch is a key on TABLE
           (push-input-char engine ch)
           (destructuring-bind (status val trace)
               (tcode-decode engine)
             (cond ((eq status 'complete)
                    (clear-strokes engine)
                    (cons val (nreverse (cons ch trace))))
                   (t
                    (cons nil nil))))))))

(define-command tcode-self-insert-command () ()
  (destructuring-bind (decoded &rest args)
      (tcode-decode-chars *tc-engine* (insertion-key-p (last-read-key-sequence)))
    (declare (ignorable args))
    (cond ((consp decoded)
           (apply (car decoded) (cdr decoded)))
          ((characterp decoded)
           (insert-character (current-point)
                             (filter *tc-engine* decoded)))
          ((functionp decoded)
           (funcall decoded))
          ((and (symbolp decoded) (lem::get-command decoded))
           (funcall decoded))
          ((eq decoded t)
           (clear-strokes *tc-engine*)))))

(defun cancel-strokes ()
  (when (tc-mode-p)
    (unless (lem::keymap-find-keybind *tc-mode-keymap*
                                      (last-read-key-sequence) nil)
      (clear-strokes *tc-engine*))))

(add-hook *post-command-hook* 'cancel-strokes)

(define-command tc-mode-help () ()
  (tcode-display-help-buffer (help-string *tc-engine*)))

(define-command toggle-alnum-mode () ()
  (toggle-alnum *tc-engine*))

(define-command tc-show-tables (seq) ("sRL,RR,LR,LL: ")
  (let ((func-pair (cond ((string-equal seq "RL") '(right-p left-p))
                         ((string-equal seq "RR") '(right-p right-p))
                         ((string-equal seq "LR") '(left-p right-p))
                         ((string-equal seq "LL") '(left-p left-p )))))
    (if func-pair
        (tcode-display-help-buffer
         (format nil "~a~%~%~a" (string-upcase seq)
                 (show-help-table (make-stroke-help-rows *tc-engine*
                                                         (first func-pair)
                                                         (second func-pair))))))))

(defun setup-tcode (file)
  (with-open-file (st file)
    (let* ((*package* (find-package :lem.tc-mode))
           (alist (read st)))
      (labels ((al2v (k)
                 (cdr (assoc k alist))))
        (setq *tc-engine*
              (make-instance 'tc-engine
                           :non-2-stroke (al2v 'non-2-stroke)
                           :special-command-alist (al2v 'special-commands)
                           :table-size (al2v 'table-size)
                           :base-table (al2v 'table)
                           :help-string (al2v 'help-string))))))
  (setup-zen-han-map)
  (loop for code from +ASCII-SPACE+ to +ASCII-TILDE+
        for char = (code-char code)
        do
           (define-key *tc-mode-keymap*
             (if (eql char #\space)
                 "Space"
                 (format nil "~c" char)) 'tcode-self-insert-command))
  (define-key *tc-mode-keymap* "?" 'tc-mode-help)

  (define-key *global-keymap* "M-\\" 'tc-mode))

