(uiop/package:define-package :lem-tcode/lemminggg (:use :cl :lem))
(in-package :lem-tcode/lemminggg)

(defvar *base-dir* (asdf:system-source-directory (asdf:find-system :lemminggg)))

(defvar *text-path*  (merge-pathnames "EELLLTXT.md" *base-dir*)
  "lemmingggの練習テキストファイル")

(defvar *lemminggg-lessons* nil)
(defconstant +lesson-size+ 700)

(defvar *regex-for-parse*
  "(?s)Lesson\\s+(\\d+):([RL][RL]!*)\\n#*\\s*Lesson-chars:\\s+(.*?)\\n`+([^`]*)")

(defconstant +dummy-char+ (code-char 0))

(defvar *lemminggg-separator* "^---")

(defvar *buffer-name* "*LEMMINGGG*")
(defvar *help-buffer-name* "*LEMMINGGG-Help*"
  "打ち方を表示するバッファ名")
(defvar *lemminggg-modeline* nil)

(defvar *lesson-input* nil)
(defvar *current-lesson* nil
  "lessonのtext")
(defvar *lesson-line* nil
  "textの一行")

(defvar *start-time* nil)
(defvar *total-time* 0)
(defvar *total-stroke* 0)
(defvar *total-error* 0)
(defvar *lesson-stroke* 0)
(defvar *lesson-error* 0)

(defvar *lemminggg-debug* nil)

(defvar *last-active-buffer* nil
  "lemmingggを開始した時のバッファ")

(defvar *quit-key-map* (make-keymap :name '*quit-key-map*
                                    :undef-hook 'lemminggg-quit))

(defstruct (lesson-number (:print-object
                           (lambda (o stream)
                             (format stream " [lesson ~d] "
                                     (lesson-number-number o)))))
  number)

(defstruct eelll-lesson
  number type chars text)

(defstruct matched-item
  position stroke order)

;;; general
(defun insert-current-point (fmt &rest args)
  (insert-string (current-point) (apply #'format nil fmt args)))

(defun sec2hms (sec)
  (let* ((hour (floor sec 3600))
         (min (floor (- sec (* hour 3600)) 60))
         (s (mod sec 60)))
    (format nil "~@[~d時間~]~@[~d分~]~d秒"
            (and (> hour 0) hour) (and (> min 0) min) s)))            

;;; cl-tcode
(defun encode-char (ch)
  (cl-tcode::tcode-encode cl-tcode::*tc-engine* ch))

(defun lemminggg-decode (stroke)
  (and stroke
       (aref (cl-tcode::table cl-tcode::*tc-engine*)
             (second stroke) (first stroke))))

(defun string-to-key-list (str)
  (loop for s across str
        collect (cl-tcode::tcode-char-to-key cl-tcode::*tc-engine* s)))

;;; global variables
(defun reset-lesson-vars ()
  (setq *lesson-stroke* 0
        *lesson-error* 0))

(defun count-up-total (time)
  (incf *total-time* time)
  (incf *total-stroke* *lesson-stroke*)
  (incf *total-error*  *lesson-error*))

(defun count-correct (correct-seq)
  (loop for c across correct-seq
        if (consp c) summing (length (second c)) into stroke
        else if (characterp c) count c into char
        finally (return (+ stroke char))))

(defun count-up-lesson-stroke (input correct)
  (let ((correct-stroke-length (count-correct correct))
        (input-length (length input)))
    (incf *lesson-stroke* input-length)
    (incf *lesson-error* (- input-length correct-stroke-length))))

;;;
(define-major-mode lemminggg-mode ()
    (:name "lemminggg"
     :keymap *lemminggg-mode-keymap*))

(defun parse-lesson (lines)
  (multiple-value-bind (match register)
      (ppcre:scan-to-strings *regex-for-parse*
                             (format nil "~{~a~^~%~}" lines))
    (when match
      (make-eelll-lesson :number (parse-integer (aref register 0))
                         :type (aref register 1)
                         :chars (aref register 2)
                         :text (remove "" (ppcre:split "\\n" (aref register 3))
                                       :test #'equal)))))

(defun set-lesson (lesson store)
  (and lesson
       (setf (aref store (eelll-lesson-number lesson)) lesson)))
             
(defun read-text-file (fname)
  (with-open-file (st fname)
    (loop for line = (read-line st nil)
          with is-data-part
          with lesson-line
          while line
          do
             (if (ppcre:scan "^## Data part" line)
                 (setq is-data-part t))
             (if is-data-part
                 (if (ppcre:scan "^-" line)
                     (progn
                       (set-lesson (parse-lesson (reverse lesson-line))
                                   *lemminggg-lessons*)
                       (setq lesson-line nil))
                     (push line lesson-line))))))

(defun lemminggg-init ()
  (setq *lemminggg-modeline* (make-lesson-number))
  (unless *lemminggg-lessons*
    (setq *lemminggg-lessons* (make-array +lesson-size+
                                          :initial-element nil))
    (read-text-file *text-path*)))


(defun lesson-exist-p (n)
  (and (> n 0)
       (< n +lesson-size+)
       (aref *lemminggg-lessons* n)))

(defun setup-lesson (&optional (number 1))
  (if (lesson-exist-p number)
      (let ((buffer (make-buffer *buffer-name*
                                     :enable-undo-p nil)))
        (setf (buffer-read-only-p buffer) nil)
        (switch-to-buffer buffer)
        (erase-buffer buffer)
        (lemminggg-mode)
        (delete-other-windows)
        (setf (lesson-number-number *lemminggg-modeline*) number)
        (modeline-add-status-list *lemminggg-modeline* buffer)
        (setq *current-lesson*
              (copy-list (eelll-lesson-text (aref *lemminggg-lessons* number))))
        (setq *start-time* nil
              *lesson-input* nil
              *lesson-stroke* 0
              *lesson-error* 0)
        (insert-string (current-point) "リターンキーを打てば始まります。 "))
      (message "lesson ~d は存在しません。" number)))

(defun find-stroke (input stroke)
  (loop for lst on input
        for position from 0
        with length = (length stroke)
        while (>= (length lst) length)
        if (equal (subseq lst 0 length) stroke)
        collect position))

(defun order-check (vector)
  (loop for v across vector
        for i from 0
        for prev = nil then (aref vector (1- i))
        if (and (consp prev) v (> (first prev) (first v)))
        do (return (list prev v))))

(defun lemminggg-match (input correct-string)
  (loop for correct across correct-string
        for stroke = (encode-char correct)
        for count from 0
        with result = (make-array (length correct-string) :initial-element nil)
        unless stroke
        do
           (setf (aref result count) correct)
        else
        do
           (loop named input-position
                 for pos in (find-stroke input stroke)
                 do
                    (cond ((loop for elm across result ; まず並びから判断
                                 always (or (null elm)
                                            (characterp elm)
                                            (and (> pos (first elm))
                                                 (> count (third elm)))))
                           (setf (aref result count)
                                 (list pos stroke count))
                           (return-from input-position))
                          (t       ; 判断を見直すベきかを判定
                           (and (not (find pos result
                                           :key #'(lambda (x)
                                                    (and (consp x)
                                                         (first x)))))
                                (loop for elm across result
                                      for i from 0
                                      if (and (consp elm)
                                              (> (first elm) pos)
                                              (> (- (first elm) (third elm))
                                                 (- pos count)))
                                      do
                                         (setf (aref result i) nil)
                                         (setf (aref result count)
                                               (list pos stroke count))
                                         (return-from input-position))))))
        finally
           (return result)))

(defun replace-matched-char (input matched)
  (loop for correct across matched
        for position = (if (listp correct) (first correct))
        for stroke = (if (listp correct) (second correct))
        if (and correct position stroke)
        do
           (replace input (cons (lemminggg-decode stroke)
                                (make-list (1- (length stroke))
                                           :initial-element +dummy-char+))
                    :start1 position)
        finally
           (return (remove +dummy-char+ input))))

(defun show-input-info (input correct-string)
  (let* ((input-string (format nil "~{~a~}" input))
         (result (lemminggg-match (string-to-key-list input-string)
                                  correct-string))
         (replaced (replace-matched-char input result)))
    (count-up-lesson-stroke input-string result)
    (if *lemminggg-debug*
        (insert-current-point "~a~%" input-string))
    (insert-current-point "~{~a~}~%" replaced)
    (loop for item across result
          for char across correct-string
          unless item collect char into wrong
          finally
             (if wrong
                 (insert-current-point "~%[間違えた字]=> ~{~a~}~%"
                                       (remove-duplicates wrong))))))

(defun current-lesson-number ()
  (or (lesson-number-number *lemminggg-modeline*)
      (setf (lesson-number-number *lemminggg-modeline*) 1)))

(defun get-current-lesson ()
  (aref *lemminggg-lessons* (current-lesson-number)))

(defun drop-head (str)
  (ppcre:regex-replace "(?s)(?:^.*?\\n)(.*)" str "\\1"))

(defun drop-top-large-row (str)
  (format nil "~{~a~%~}" (subseq (ppcre:split "\\n" str) 5)))

(defun drop-each-small-top (str)
  (ppcre:regex-replace-all "(?s)(\\n\\n).*?\\n"
                           (ppcre:regex-replace "(?s)^.*?\\n" str "")
                           "\\1"))

(defun drop-top-rows (str)
  (drop-each-small-top (drop-top-large-row str)))

(defun lemminggg-make-help (type chars &optional whole)
  (let ((text (ppcre:regex-replace-all (format nil "[^~a \\n]" chars)
                                       (cl-tcode:make-stroke-help type)
                                       "- ")))
    (if whole
        text
        (drop-top-rows text))))

(defun help-window-size (type)
  (if (find #\! type) 20 12))

(defun lemminggg-show-help (lesson)
  (let ((type (eelll-lesson-type lesson))
        (chars (eelll-lesson-chars lesson)))
    (split-window-vertically (current-window) (help-window-size type))
    (other-window)
    (cl-tcode:tcode-display-help-buffer (lemminggg-make-help type chars)
                                        *help-buffer-name*)
    (let ((buffer (get-buffer *help-buffer-name*)))
      (modeline-clear-status-list buffer)
      (modeline-add-status-list (format nil " (~c->~c)" (elt type 0) (elt type 1))
                                buffer)
    (modeline-add-status-list *lemminggg-modeline* buffer))))

(defun display-score-1 (time stroke error)
  (unless (zerop stroke)
    (insert-current-point "~%~%(総打鍵成績)毎打鍵 ~4,2f秒(毎分 ~4,1f打鍵)~%"
                          (/ time stroke)
                          (* (/ stroke time) 60))
    (unless (zerop (- stroke error))
      (insert-current-point "(実打鍵成績)毎打鍵 ~4,2f秒(毎分 ~4,1f打鍵)~%"
                            (/ time (- stroke error))
                            (* (/ (- stroke error) time) 60)))
    (insert-current-point "          エラーレート ~5,2f%~%"
                          (* (/ error stroke) 100))))

(defun display-score  (elapsed-time)
  (unless (zerop elapsed-time)
    (display-score-1 elapsed-time *lesson-stroke* *lesson-error*)))

(defun display-total-score ()
  (unless (zerop *total-stroke*)
    (insert-current-point "総合成績~%~%")
    (display-score-1 *total-time* *total-stroke* *total-error*)
    (insert-current-point "     入力打鍵数 ~d 打鍵 所要時間 ~a"
                          *total-stroke* (sec2hms *total-time*))))

(defun next-lesson ()
  (or (loop for n from (1+ (current-lesson-number)) to +lesson-size+
            if (aref *lemminggg-lessons* n)
            do
               (setup-lesson n)
               (return t))
      (error "no more lessons~%")))  

(defun end-lemminggg ()
  (erase-buffer (current-buffer))
  (display-total-score)
  (setf (mode-keymap 'lemminggg-mode) *quit-key-map*)
  (insert-current-point "~%~%おつかれさまでした。どれかキーを押してください。"))  

(defun end-lesson ()
  (let ((elapsed-time (- (get-universal-time) *start-time*)))
    (count-up-total elapsed-time)
    (display-score elapsed-time)
    (insert-current-point "~%~%lesson ~dが終わりました。"
                          (current-lesson-number))
    (window-see (current-window))
    (lem-select-popup:start-select-popup
     `(("もう一度トライする" . ,#'(lambda (arg)
                                    (declare (ignore arg))
                                    (setup-lesson (current-lesson-number))))
       ("次のlessonに進む" . ,#'(lambda (arg)
                                  (declare (ignore arg))
                                  (next-lesson)))
       ("lemminggg(ねずみみみ)を終了する" . ,#'(lambda (arg)
                                                 (declare (ignore arg))
                                                 (end-lemminggg)))))))

(define-command lemminggg-return () ()
  (if *start-time*
      (progn
        (show-input-info (reverse *lesson-input*) *lesson-line*)
        (setq *lesson-input* nil))
      (progn
        (insert-current-point
         "~%==============================================================~%")
        (lemminggg-show-help (get-current-lesson))
        (setq *start-time* (get-universal-time))))
  (if *current-lesson*
      (insert-current-point "~%~a~%"
                            (setq *lesson-line* (pop *current-lesson*)))
      (end-lesson)))

;;; todo
(define-command lemminggg-help () ()
  )

(define-command lemminggg-delete () ()
  (display-popup-message "間違いを気にせずどんどん入力してください。"
                         :timeout 1))

(define-command lemminggg-quit () ()
  (fundamental-mode)
  (setf (buffer-read-only-p (current-buffer)) t)
  (alexandria:if-let (win (car (get-buffer-windows
                                (get-buffer *help-buffer-name*))))
    (delete-window win))
  (if *last-active-buffer*
      (switch-to-buffer *last-active-buffer*))
  (setq *last-active-buffer* nil))

(define-command lemminggg-confirm-quit () ()
  (if (prompt-for-y-or-n-p "lemminggg(ねずみみみ)を終了しますか？")
    (lemminggg-quit)))

(define-command lemminggg-key () ()
  (push (insertion-key-p (last-read-key-sequence)) *lesson-input*))

(defun completion-lesson-list (str)
  (loop for lesson across *lemminggg-lessons*
        if (and lesson
                (ppcre:scan (format nil "^~d" str)
                            (format nil "~d" (eelll-lesson-number lesson))))
        collect (format nil "~d: ~a"
                        (eelll-lesson-number lesson)
                        (car (eelll-lesson-text lesson)))))

(defun prompt-for-lesson ()
  (or *lemminggg-modeline*
      (lemminggg-init))
  (parse-integer (prompt-for-line "練習テキスト: "
                                  nil
                                  #'completion-lesson-list
                                  #'(lambda (str)
                                      (parse-integer str :junk-allowed t))
                                  'mh-read-number)
                 :junk-allowed t))

(define-command lemminggg (n) ((list (prompt-for-lesson)))
  (setq *last-active-buffer* (current-buffer))
  (setf (mode-keymap 'lemminggg-mode) *lemminggg-mode-keymap*)
  (setup-lesson n))

;;; set key
(loop for code from cl-tcode::+ASCII-SPACE+ to cl-tcode::+ASCII-TILDE+
      for char = (code-char code)
      do
         (define-key *lemminggg-mode-keymap*
           (if (eql char #\space)
               "Space"
               (cl-tcode:char-to-string char)) 'lemminggg-key))
(define-key *lemminggg-mode-keymap* "Return" 'lemminggg-return)
(define-key *lemminggg-mode-keymap* "C-j" 'lemminggg-return)
(define-key *lemminggg-mode-keymap* "C-m" 'lemminggg-return)
(define-key *lemminggg-mode-keymap* "Backspace" 'lemminggg-delete)
(define-key *lemminggg-mode-keymap* "Delete" 'lemminggg-delete)
(define-key *lemminggg-mode-keymap* "C-h" 'lemminggg-delete)
(define-key *lemminggg-mode-keymap* "C-g" 'lemminggg-confirm-quit)
