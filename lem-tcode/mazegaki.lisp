(uiop/package:define-package :lem-tcode/mazegaki
  (:use :cl :lem)
  (:export :mazegaki-begin-conversion :mazegaki-finish :mazegaki-mode-p
           :clear-help))

(in-package :lem-tcode/mazegaki)

(defvar *mazegaki-max-suffix-length* 4
  "読みの中の活用語尾の最大文字数。")

(defvar *use-floating-window* t)

;;; overlay
(defvar *tcode-overlay* nil)

(define-attribute conversion
  (t :underline-p t))

(define-attribute inflection
  (t :reverse-p t))

(defun set-overlay (start end)
  (setq *tcode-overlay*
        (make-overlay start end 'conversion)))

(define-command clear-overlay () ()
  (and *tcode-overlay*
       (delete-overlay *tcode-overlay*)
       (setq *tcode-overlay* nil)))

(defvar *tcode-mazegaki-stroke-priority-list*
; キー配置
;  0  1  2  3  4    5  6  7  8  9
; 10 11 12 13 14   15 16 17 18 19
; 20 21 22 23 24   25 26 27 28 29
; 30 31 32 33 34   35 36 37 38 39
  '(22 23 21 24 20
    12 13 11 14 10
    27 26 28 25 29
    17 16 18 15 19)
  "* 候補を並べるときの位置。このリストにないキーは使用されない。")

(defvar *tcode-mazegaki-terminate-char-list*
  (mapcar (lambda (ch) (cl-tcode:string-to-char ch))
            '("、" "。" "，" "．" "・" "「" "」" "（" "）"))
  "* 交ぜ書き変換の読みに含まれない2バイト文字のリスト。")

(define-command mazegaki-use-floating-window () ()
  (message "~a" (setq *use-floating-window* (not *use-floating-window*))))

(define-minor-mode tc-mazegaki-mode
    (:name "mazegaki"
     :keymap *tc-mazegaki-keymap*))

(defun mazegaki-mode-p ()
  (mode-active-p (current-buffer) 'tc-mazegaki-mode))

(defun clear-help ()
  (lem-tcode/help-buffer:remove-help-buffer)
  (lem.popup-window::clear-popup-message))

(defun get-mazegaki-line (point)
  (with-point ((s point))
    (line-start s)
    (points-to-string s point)))

(defun mazegaki-yomi-list (point)
  (loop for ch across (get-mazegaki-line point)
        for pos from 0
        with contents
        do
           (push (cons pos ch) contents)
        finally
           (return contents)))

(defun mazegaki-construct-yomi (yomi-list length &optional (suffix 0))
  (concatenate 'string
               (reverse (mapcar #'cdr
                                (subseq yomi-list suffix length)))))

(defun mazegaki-construct-suffix (yomi-list suffix)
  (mazegaki-construct-yomi yomi-list suffix))

(defun use-whole-table (whole-page candidate-table)
  (or (> whole-page 1)  ;ページ数が1より大きい
      ;; 3段目以外に候補がある
      (loop for word across candidate-table
            for i from 0
            do
               (if (and word
                        (or (< i 20) (>= i 30)))
                   (return t)))))

(defun show-candidate-in-minibuffer (candidate-table page whole-page
                                     &optional msg suffix)
  (message (format nil "~@{~@[~a~]~}"
                   msg
                   (if (= whole-page 1)
                       ""
                       (format nil "(~d/~d)  " page whole-page))
                   (format nil "~{[~a ~a ~a ~a] ~a  ~a [~a ~a ~a ~a]~}"
                           (loop for n from 20 to 29
                                 collect (or (aref candidate-table n) "-")))
                   "  "
                   suffix)))

(defun show-using-buffers (candidate-table page whole-page &optional msg suffix)
  (if (use-whole-table whole-page candidate-table)
      (progn
        (setq msg (format nil "~a ~@[~a~]" msg suffix))
        (if (not (minibuffer-window-p (current-window)))
            (message msg ""))
        (lem-tcode/help-buffer:display-help-buffer
         (cl-tcode:tcode-draw-table candidate-table page whole-page)))
      (show-candidate-in-minibuffer candidate-table page whole-page
                                    msg suffix)))

(defun show-using-floating-window (candidate-table page whole-page
                                    &optional msg suffix)
  (setq msg (format nil "~a ~@[~a~]" msg suffix))
  (lem-if:display-popup-message (implementation)
                                (cl-tcode:tcode-draw-table candidate-table
                                                           page whole-page)
                                nil))

(defun show-candidate-not-inline (candidate-table noc current-offset
                                  &optional msg suffix)
  "candidate-table から候補を表示する。
noc (候補の数)と current-offset から現在何番目の表を表示しているか計算する。"
  (let* ((plist-size (length *tcode-mazegaki-stroke-priority-list*))
         (whole-page (floor (/ (+ noc (1- plist-size)) plist-size)))
        (page (- (1+ whole-page)
                  (floor (/ (+ (- noc current-offset) (1- plist-size))
                            plist-size)))))
    (if *use-floating-window*
        (show-using-floating-window candidate-table page whole-page msg suffix)
        (show-using-buffers candidate-table page whole-page msg suffix))))

(defun mazegaki-make-candidate-table (candidate-list)
  "candidate-listから候補の表を作る。
候補の表における位置は、定数 `*tcode-mazegaki-stroke-priority-list*' に従う。"
  (loop with table = (make-array (cl-tcode:get-table-size) :initial-element nil)
        for candidate in candidate-list
        for position in *tcode-mazegaki-stroke-priority-list*
        do
           (setf (aref table position) candidate)
        finally
           (return table)))

(defclass mazegaki-converter ()
  ((yomi
    :initarg :yomi
    :reader mzgk-yomi
    :documentation "読み: (position . 文字) のalist")
   (point
    :accessor mzgk-point
    :documentation "yomiの変換開始ポイント")
   (length
    :initarg :length
    :reader mzgk-len
    :documentation "yomiの変換対象の長さ")
   (suffix
    :initform 0
    :reader mzgk-suffix
    :documentation "活用語尾の長さ")
   (inflection-only
    :initform nil
    :documentation "活用変換中かどうか")
   (candidate
    :reader mzgk-candidate
    :documentation "変換候補全てのリスト")
   (noc
    :reader mzgk-noc
    :documentation "候補の数")
   (current
    :accessor mzgk-current
    :documentation "表示している候補: リストあるいは配列")
   (kakutei
    :initform nil
    :reader mzgk-kakutei
    :documentation "変換を確定した漢字")
   (help-offset
    :initform 0
    :accessor mzgk-offset
    :documentation "表示している候補のoffset")))

(defmethod set-candidate ((converter mazegaki-converter) candidate-list)
  (with-slots (candidate current noc) converter
    (setf candidate candidate-list
          noc (length candidate-list))
    (setf current
          (if (> noc 1)
              (mazegaki-make-candidate-table candidate-list)
              candidate-list))))

(defmethod set-kakutei ((converter mazegaki-converter) &optional inserted)
  (with-slots (kakutei noc candidate) converter
    (cond (inserted
            (setf kakutei inserted))
          ((= noc 1)
           (setf kakutei (car candidate))))))

(defmethod set-point ((converter mazegaki-converter))
  (with-slots (point) converter
    (setf point (copy-point (current-point) :right-inserting))
    (character-offset point (- (slot-value converter 'length)))))

(defmethod reset-point ((converter mazegaki-converter))
  (when (slot-boundp converter 'point)
    (delete-point (slot-value converter 'point))
    (slot-makunbound converter 'point)))

(defmethod set-length ((converter mazegaki-converter) len &optional (sx 0))
  ;; 初回のみ特別扱い/未設定なら設定する
  (unless (slot-boundp converter 'point)
    (set-point converter))

  (with-slots (length point suffix) converter
    (character-offset point (- length len))
    (setf length len
          suffix sx)))

(defmethod max-length-p ((converter mazegaki-converter))
  (with-slots (length yomi) converter
    (>= length (length yomi))))

(defmethod inflection-p ((converter mazegaki-converter))
  (slot-value converter 'inflection-only))

(defmethod reset-for-inflection ((converter mazegaki-converter) inflection)
  (with-slots (length suffix yomi inflection-only) converter
    (setf suffix 0)
    (if inflection
        (setf length (length yomi)
              inflection-only t)
        (setf length 1
              inflection-only nil))))

(defmethod start-inflection ((converter mazegaki-converter))
  (reset-for-inflection converter t))

(defmethod stop-inflection ((converter mazegaki-converter))
  (reset-for-inflection converter nil))

(defun debug-point (converter msg)
  (message "~s len: ~d, point: ~a" msg
           (slot-value converter 'length)
           (if (slot-boundp converter 'point)
               (slot-value converter 'point))))

(defmethod mzgk-construct-suffix ((converter mazegaki-converter))
  (with-slots (yomi suffix length) converter
    (list (mazegaki-construct-yomi yomi length suffix)
          (mazegaki-construct-yomi yomi suffix))))

(defmethod next-candidates ((converter mazegaki-converter))
  (with-slots (candidate noc current help-offset) converter
    (let ((new-offset (+ help-offset
                         (length *tcode-mazegaki-stroke-priority-list*))))
      (setf help-offset
            (if (>= new-offset noc)
                0
                new-offset))
      (setf current (mazegaki-make-candidate-table (nthcdr help-offset
                                                           candidate))))))

;;; converter function
;;; begin
(defun mazegaki-lookup (converter &optional (delta 0))
  "現在の読みより短い最長の読みを探す。"
  (let ((yomi (mzgk-yomi converter))
        (length (mzgk-len converter)))
    (and (not (inflection-p converter))
         (loop for len from (- length delta) downto 0
               for word = (mazegaki-construct-yomi yomi len)
               for candidate-list = (cl-tcode:lookup-mazegaki-dic word)
               do
                  (when candidate-list
                    (set-length converter len)
                    (set-candidate converter candidate-list)
                      (return t))))))

(defun mazegaki-lookup-reverse (converter)
  "現在の読みよりも長い最短の読みを見つける。"
  (cond ((inflection-p converter)
         (stop-inflection converter)
         ;; pointを再設定する
         (reset-point converter)
         (set-point converter))
        (t
         (incf (slot-value converter 'length))))

  (let* ((yomi (mzgk-yomi converter))
         (length (mzgk-len converter))
         (max-len (length yomi)))
    (loop for len from length to max-len
          for word = (mazegaki-construct-yomi yomi len)
          for candidate-list = (cl-tcode:lookup-mazegaki-dic word)
          do
             (when candidate-list
               (set-length converter len)
               (set-candidate converter candidate-list)
               (return t)))))

(defun dec-length (converter &optional cl cs)
  (let ((suffix (mzgk-suffix converter))
        (length (mzgk-len converter)))
    (cond ((or (null cs) (null cl))
           (list length suffix))
          ((or (> cl 0))
           (if (>= cs (min *mazegaki-max-suffix-length* (1- cl)))
               (list (1- cl) 0)
               (list cl (1+ cs)))))))

(defun inc-length (converter &optional cl cs)
  (let ((suffix (mzgk-suffix converter))
        (length (mzgk-len converter))
        (max-length (length (mzgk-yomi converter))))
    (cond ((or (null cs) (null cl))
           (list length suffix t))
          ((or (< cl max-length) (> cs 0))
           (cond ((zerop cs)
                  (list (1+ cl) (min *mazegaki-max-suffix-length* cl) t))
                 (t (list cl (1- cs) t))))
          ;; これ以上伸ばせない
          (t (list cl cs nil)))))

(defun mazegaki-lookup-with-inflection (converter &optional shorter)
  "現在の読みより短い、活用する最長の読みを探す。"
  ;; lengthとsuffixの設定
  (when (null (inflection-p converter))
    (start-inflection converter)
    ;; pointを再設定する
    (reset-point converter)
    (set-point converter))

  (let ((yomi (mzgk-yomi converter))
        (length (mzgk-len converter))
        (suffix (mzgk-suffix converter)))
    (loop for (l s) = (if shorter
                          (dec-length converter length suffix)
                          (dec-length converter))
          then (dec-length converter l s)
          for word = (mazegaki-construct-yomi yomi l s)
          for candidate-list = (cl-tcode:lookup-mazegaki-dic word t)
          while (> l 0)
          do
             (when candidate-list
               (set-length converter l s)
               (set-candidate converter candidate-list)
               (return t)))))

(defun mazegaki-lookup-with-inflection-reverse (converter)
  "現在の読みよりも長い最短の活用する読みを見つける。"
  (and (inflection-p converter)
       (let ((yomi (mzgk-yomi converter))
             (length (mzgk-len converter))
             (suffix (mzgk-suffix converter)))
         (loop for (l s f) = (inc-length converter length suffix)
               then (inc-length converter l s)
               for word = (mazegaki-construct-yomi yomi l s)
               for candidate-list = (cl-tcode:lookup-mazegaki-dic word t)
               while f
               do
                  (when candidate-list
                  (set-length converter l s)
                  (set-candidate converter candidate-list)
                  (return t))))))

(defun mazegaki-erase-previous-candidate (converter)
  (with-point ((s (mzgk-point converter))
               (e (current-point)))
    (delete-between-points s e)))

(defun insert-the-candidate (candidate &optional suffix yomi-list)
  (let ((str (if suffix
                 (format nil "~a~a"
                         candidate (mazegaki-construct-suffix yomi-list
                                                              suffix))
                 candidate)))
    (insert-string (current-point) str)
    str))

(defun mazegaki-show-candidate-inline (converter candidate)
  (mazegaki-erase-previous-candidate converter)
  (let ((position (point-charpos (current-point))))
    (if (listp candidate)
        (if (= (length candidate) 1)
            (set-kakutei converter
                         (insert-the-candidate (car candidate)
                                               (mzgk-suffix converter)
                                               (mzgk-yomi converter)))
            (message "displaying more than 2 words is not yet supported."))
        (set-kakutei converter
                     (insert-the-candidate candidate
                                           (mzgk-suffix converter)
                                           (mzgk-yomi converter))))
    (with-point ((begin (current-point)))
      (setf (point-charpos begin) position)
      (setf (mzgk-point converter) begin))))

(defun mazegaki-make-table-and-select (converter &optional msg inline)
  "現在の読みから候補を選択させ、その文字列または文字(キー)を返す。
   候補が一つの時はリターンで確定する。"
  (declare (ignorable inline))
  ;; 表示
  (case (mzgk-noc converter)
    (1
     (mazegaki-show-candidate-inline converter (mzgk-candidate converter)))
    (otherwise
     (show-candidate-not-inline (mzgk-current converter) (mzgk-noc converter)
                                (mzgk-offset converter)
                                msg
                                (if (inflection-p converter)
                                    (format nil "~{~a(~a)~}"
                                            (mzgk-construct-suffix converter))))))
  ;; 選択
  (tc-mazegaki-mode t))

(defun show-yomi (converter)
  (let ((point (mzgk-point converter))
        (length (mzgk-len converter)))
    ;; show underline
    (clear-overlay)
    (with-point ((s point)
                 (e point))
      (character-offset e length)
      (set-overlay s e))))

(defun mazegaki-select-candidate (converter)
  (show-yomi converter)
  ;; test implementation
  (mazegaki-make-table-and-select converter nil t))

(defun show-next-candidates (converter)
  (next-candidates converter)
  (show-candidate-not-inline (mzgk-current converter) (mzgk-noc converter)
                             (mzgk-offset converter)))

(defun reset-yomi (converter)
  (mazegaki-erase-previous-candidate converter)
  (insert-string (current-point)
                 (mazegaki-construct-yomi (mzgk-yomi converter)
                                          (mzgk-len converter)))
  (reset-point converter))
;;; converter fuction
;;; end

(let ((mazegaki-converter))
  (defun set-mazegaki-converter (converter)
    (setf mazegaki-converter converter))

  (defun mazegaki-finish ()
    (clear-overlay)
    (lem.popup-window::clear-popup-message)
    (if mazegaki-converter
        (show-converted-stroke (mzgk-kakutei mazegaki-converter)
                               (mazegaki-construct-yomi
                                (mzgk-yomi mazegaki-converter)
                                (mzgk-len mazegaki-converter))))
    (set-mazegaki-converter nil)
    (tc-mazegaki-mode nil))

  (defun mazegaki-show/redo (selected)
    (cond ((stringp selected)
           ;; 漢字を表示して終わる。
           (mazegaki-show-candidate-inline mazegaki-converter selected)
           (mazegaki-finish))
          ((characterp selected)
           ;; not yet
           t)
          (t
           (message "mazegaki error: read ~a" selected)
           (mazegaki-finish))))

  (defun mazegaki-execute-select ()
    (let* ((ch (insertion-key-p (last-read-key-sequence)))
           (key (cl-tcode:tcode-char-to-key cl-tcode:*tc-engine* ch)))
      (message "")  ; clear minibuffer
      (mazegaki-show/redo (if (minusp key)
                              ch
                              (or (aref (mzgk-current mazegaki-converter) key)
                                  ch)))))

  ;; command
  (define-command mazegaki-convert () ()
    (let* ((point (current-point))
           (yomi-list (mazegaki-yomi-list point)))
      (set-mazegaki-converter (make-instance 'mazegaki-converter
                                             :yomi yomi-list
                                             :length (length yomi-list)))
      (if (mazegaki-lookup mazegaki-converter)
          (mazegaki-select-candidate mazegaki-converter)
          (message "適当な漢字はありません"))))

  (define-command mazegaki-select-command () ()
    (case (mzgk-noc mazegaki-converter)
      (1
       (unread-key-sequence (last-read-key-sequence))
       (set-kakutei mazegaki-converter)
       (mazegaki-finish))
      (otherwise
       (mazegaki-execute-select))))

  (define-command mazegaki-cancel () ()
    (mazegaki-erase-previous-candidate mazegaki-converter)
    (with-slots (yomi length) mazegaki-converter
      (insert-string (current-point)
                     (mazegaki-construct-yomi yomi length)))
    (mazegaki-finish))

  (define-command mazegaki-cancel/set () ()
    (if (= (mzgk-noc mazegaki-converter) 1)
        (mazegaki-finish)
        (mazegaki-cancel)))

  (define-command mazegaki-next-candidate () ()
    (show-next-candidates mazegaki-converter))

  (define-command mazegaki-relimit-right () ()
    "読みを縮める。"
    (clear-help)
    (reset-yomi mazegaki-converter)
    (let ((current (mzgk-len mazegaki-converter)))
      (if (or (mazegaki-lookup mazegaki-converter 1)
              (mazegaki-lookup-with-inflection mazegaki-converter t))
          (mazegaki-select-candidate mazegaki-converter)
          (progn
            (message "これ以上読みは縮められません。")
            (set-length mazegaki-converter current)
            (show-yomi mazegaki-converter)))))

  (define-command mazegaki-relimit-left () ()
    "読みを伸ばす。"
    (clear-help)
    (reset-yomi mazegaki-converter)
    (let ((current (mzgk-len mazegaki-converter)))
      (if (or (mazegaki-lookup-with-inflection-reverse mazegaki-converter)
              (mazegaki-lookup-reverse mazegaki-converter))
          (mazegaki-select-candidate mazegaki-converter)
          (progn
            (message "これ以上読みは伸ばせません。")
            (set-length mazegaki-converter current)
            (show-yomi mazegaki-converter)))))
  )

(define-command mazegaki-begin-conversion () ()
  (mazegaki-convert))

(loop for char in cl-tcode:*tcode-char-list*
      do
         (define-key *tc-mazegaki-keymap*
           (format nil "~c" char) 'mazegaki-select-command))

(define-key *tc-mazegaki-keymap* "C-c" 'mazegaki-cancel)
(define-key *tc-mazegaki-keymap* "Space" 'mazegaki-next-candidate)
(define-key *tc-mazegaki-keymap* ">" 'mazegaki-relimit-right)
(define-key *tc-mazegaki-keymap* "<" 'mazegaki-relimit-left)
(define-key *tc-mazegaki-keymap* "C-m" 'mazegaki-cancel/set)

(defun display-direct-stroke (engine kakutei &optional yomi)
  "KAKUTEI の中で、 YOMI に含まれず、かつ直接入力できる漢字を表示する。"
  (let* ((target (if yomi
                    (loop for c across yomi
                          for result = (remove c kakutei)
                          then (remove c result)
                          finally
                             (return result))
                    kakutei))
         (drawing (loop for ch across (remove-duplicates target)
                        if (cl-tcode:show-stroke engine ch) collect it))
         (stroke (if drawing (format nil "~{~a~%~}" drawing))))
    (and stroke
         (if *use-floating-window*
             (lem-if:display-popup-message (implementation) stroke nil)
             (lem-tcode/help-buffer:display-help-buffer stroke)))))

(defun show-converted-stroke (kakutei &optional yomi)
  (and kakutei
       (display-direct-stroke cl-tcode:*tc-engine* kakutei yomi)))

(define-command tcode-query-stroke () ()
  (alexandria:if-let ((drawing (cl-tcode:show-stroke cl-tcode:*tc-engine*
                                                     (character-at (current-point)))))
    (lem-tcode/help-buffer:display-help-buffer drawing)
    (message "ストロークはありません。")))
