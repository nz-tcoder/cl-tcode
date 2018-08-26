(in-package :cl-tcode)

(defun left-p (key)
  (< (rem key 10) 5))

(defun right-p (key)
  (>= (rem key 10) 5))

(defun make-key-list (func)
  (loop for k from 0 to 39
        if (funcall func k) collect k))

(defmacro summarize-by-n (n lst func)
  (let ((gs (loop repeat n collect (gensym))))
    `(loop for ,gs on ,lst by #'(lambda (x) (nthcdr ,n x))
           collect (funcall ,func (list ,@gs)))))

(defun summarize-by-5 (lst &optional (func #'identity))
  (summarize-by-n 5 lst func))

(defun non-char-to-asterisk (x)
  (cond ((characterp x)
         (if (or (eql x #\■) (eql x #\◆))
             #\＊
             x))
        (t #\＊)))

(defun char-list-to-string (lst &optional (func #'identity))
  (concatenate 'string (mapcar func lst)))

(defun make-stroke-help-rows (engine first second)
  (summarize-by-5
   (mapcar #'(lambda (x)
               (summarize-by-5 (mapcar #'(lambda (y)
                                           (aref (table engine) x y))
                                       (make-key-list first))))
           (make-key-list second))))

(defun stroke-char-to-string (char-list)
  (char-list-to-string (mapcar #'non-char-to-asterisk
                               (mapcar #'to-zenkaku char-list))))

(defun show-each-row (rows stream)
  (loop for n from 0 to 3
        for line = (mapcar #'(lambda (x) (stroke-char-to-string (nth n x)))
                           rows)
        do
           (format stream "~{~a~^  ~}~%" line)))

(defun make-stroke-help-table (matrix)
  (loop with st = (make-string-output-stream)
        for rows in matrix
        do
             (show-each-row rows st)
             (format st "~%")
        finally
           (return (get-output-stream-string st))))

(defun make-stroke-help-1 (engine type)
  (let ((func-pair (cond ((string-equal type "RL") '(right-p left-p))
                         ((string-equal type "RR") '(right-p right-p))
                         ((string-equal type "LR") '(left-p right-p))
                         ((string-equal type "LL") '(left-p left-p )))))
    (if func-pair
        (make-stroke-help-table (make-stroke-help-rows engine
                                                       (first func-pair)
                                                       (second func-pair))))))

(defun make-stroke-help (type)
  (make-stroke-help-1 *tc-engine* type))

(defvar *help-stroke-symbol-list* '(#\●  #\○  #\△ #\◇))
(defvar *help-double-stroke-symbol* #\◎)
(defvar *help-another-double-stroke-symbol* #\☆)

(defun key-position (key)
  (multiple-value-list (floor key 10)))

(defun make-seq (lst length default special)
  (loop repeat length
        for i from 0
        if (find i lst :key #'car)
          collect (char-to-string (cadr (find i lst :key #'car))) into result
        else if (find i special) collect "  " into result
        else collect default into result
        finally (return result)))

(defun make-stroke-seq (lst default)
  (make-seq lst 10 default '(4 5)))

(defstruct draw-info
  key symbol order)

(defun update-draw-info (info order used-double-symbol)
  (when (find (draw-info-symbol info) *help-stroke-symbol-list*)
    (setf (draw-info-symbol info)
          (cond ((null used-double-symbol)  *help-double-stroke-symbol*)
                ((eql (draw-info-symbol used-double-symbol)
                      *help-double-stroke-symbol*)
                 *help-another-double-stroke-symbol*)
                (t *help-double-stroke-symbol*))))
  (push order (draw-info-order info)))

(defun unused-symbol (vector)
  (loop for s in *help-stroke-symbol-list*
        do
        (unless (find s vector :key #'draw-info-symbol)
          (return s))))

(defun stroke->draw-info (stroke)
  (loop for s in stroke with info-vector = (make-array 4 :fill-pointer 0)
        for count from 0
        do
           (alexandria:if-let ((multi (find s info-vector
                                            :key #'draw-info-key)))
             (if (find (draw-info-symbol multi) *help-stroke-symbol-list*)
                 (update-draw-info multi count
                                   (find *help-double-stroke-symbol*
                                         info-vector
                                         :key #'draw-info-symbol)))
             (vector-push (make-draw-info :key s
                                          :symbol (unused-symbol info-vector)
                                          :order (list count))
                          info-vector))
        finally
           (return info-vector)))

(defun info->drawing-list (info-array row)
  (loop for info across info-array
        for (r c) = (key-position (draw-info-key info))
        collect (list (if (= r row) c -1) (draw-info-symbol info))))

(defun symbol-and-order-1 (info-array)
  (loop for info across info-array
        for sym = (draw-info-symbol info)
        for order = (reverse (draw-info-order info))
        collect (list order sym)))

(defun symbol-and-order (info-array)
  (let ((lst (sort (symbol-and-order-1 info-array)
                   #'(lambda (x y)
                       (< (apply #'min x) (apply #'min y)))
                   :key #'first)))
    (loop for (order sym) in lst
          collect (format nil "~c…~{第~d~^、~}打鍵"
                          sym (mapcar #'1+ order)))))

(defun stroke-drawing (stroke)
  "strokeの打ち方を表す図(実はリスト)を返す。strokeはkeyのリスト"
  (if stroke
      (let* ((draw-info (stroke->draw-info stroke))
             (top-row (make-stroke-seq (info->drawing-list draw-info 0) "  ")))
        (values
         (cons top-row
               (loop repeat 3 for row from 1
                     collect (make-stroke-seq (info->drawing-list draw-info row)
                                              "・")))
         (symbol-and-order draw-info)))
      (values nil nil)))

(defun stroke-order-padding (lst)
  (case (length lst)
    (1 (append lst (list "")))
    (2 lst)
    (3 (list (format nil "~a ~a" (first lst) (third lst)) (second lst)))
    (t (list (format nil "~a ~a" (first lst) (third lst))
             (format nil "~a ~a" (second lst) (fourth lst))))))

(defun stroke-list->string (ch drawing-list symbol-order)
  (format nil "~{~{~{~a ~a~}   ~a~}~%~}"
          (mapcar #'list
                  (mapcar #'(lambda (x)
                              (summarize-by-5 x #'(lambda (y)
                                                    (format nil "~{~a~}" y))))
                          drawing-list)
                  `(,(char-to-string ch) "" ,@(stroke-order-padding
                                               symbol-order)))))

(defun show-stroke (engine ch)
  (multiple-value-bind (drawing-list symbol-order)
      (stroke-drawing (tcode-encode engine ch))
    (and drawing-list
         (stroke-list->string ch drawing-list symbol-order))))

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
                        if (show-stroke engine ch) collect it)))
    (if drawing
        (tcode-display-help-buffer (format nil "~{~a~%~}" drawing)))))

;;; call in mazegaki.lisp
(defun show-converted-stroke (kakutei &optional yomi)
  (and kakutei
       (display-direct-stroke *tc-engine* kakutei yomi)))

(define-command tcode-query-stroke () ()
  (alexandria:if-let ((drawing (show-stroke *tc-engine*
                                          (character-at (current-point)))))
    (tcode-display-help-buffer drawing)
    (message "ストロークはありません。")))
