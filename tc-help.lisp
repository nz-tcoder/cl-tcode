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

(defun show-help-table (matrix)
  (loop with st = (make-string-output-stream)
        for rows in matrix
        do
             (show-each-row rows st)
             (format st "~%")
        finally
           (return (get-output-stream-string st))))
        
