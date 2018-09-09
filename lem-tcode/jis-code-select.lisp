(uiop/package:define-package :lem-tcode/jis-code-select
  (:use :cl :lem)
  (:export :tcode-jis-code-select))

(in-package :lem-tcode/jis-code-select)

(defvar *jis-code-select-buffer-name* "*JIS code*")
(defvar *target-buffer* nil)

(defun get-from-table (code)
  (alexandria:if-let (ch (gethash code lem-encodings/euc-jp::*from*))
    ch
    (gethash (+ (* #x8f #x10000) code) lem-encodings/euc-jp::*from*)))

(defun make-jis-code-rows ()
  (cl-tcode:summarize-by-n
   #x20
   (loop for ku from #x21 to #x7e
         append (loop for ten from #x21 to #x7e
                      for code = (+ (* ku #x100) ten)
                      for euc = (boole boole-ior code #x8080)
                      collect (if (get-from-table euc)
                                  (code-char (get-from-table euc)))))
   #'(lambda (x) (format nil "~{~@[~c~]~}" x))))

(defun make-jis-code-table ()
  (loop with st = (make-string-output-stream)
        for l in (make-jis-code-rows)
        for head from #x2121 by #x20
        if (> (length l) 0)
        do
          (format st "~x ~a~%" head l)
        finally
           (return (get-output-stream-string st))))

(define-command tcode-insert-selected-jis-code-char () ()
  (when (and *target-buffer* (get-buffer *target-buffer*))
    (insert-string (buffer-point *target-buffer*)
                   (format nil "~c" (character-at (current-point))))))

(define-command tcode-quit-jis-select () ()
  (delete-window (display-buffer (get-buffer *jis-code-select-buffer-name*)))
  (setq *target-buffer* nil))

(define-major-mode tc-jis-code-select-mode ()
    (:name "jis-code-select-mode"
     :keymap *jis-code-select-mode-keymap*))

(loop for code from cl-tcode::+ASCII-SPACE+ to cl-tcode::+ASCII-TILDE+
      for char = (code-char code)
      do
         (define-key *jis-code-select-mode-keymap*
           (if (eql char #\space)
               "Space"
               (cl-tcode:char-to-string char))
           'do-nothing))

(define-key *jis-code-select-mode-keymap* "Return" 'tcode-insert-selected-jis-code-char)
(define-key *jis-code-select-mode-keymap* "C-j" 'tcode-insert-selected-jis-code-char)
(define-key *jis-code-select-mode-keymap* "C-m" 'tcode-insert-selected-jis-code-char)

(define-key *jis-code-select-mode-keymap* "C-h" 'previous-page)
(define-key *jis-code-select-mode-keymap* "Backspace" 'previous-page)
(define-key *jis-code-select-mode-keymap* "Delete" 'previous-page)
(define-key *jis-code-select-mode-keymap* "Space" 'next-page)
(define-key *jis-code-select-mode-keymap* "h" 'backward-char)
(define-key *jis-code-select-mode-keymap* "j" 'next-line)
(define-key *jis-code-select-mode-keymap* "k" 'previous-line)
(define-key *jis-code-select-mode-keymap* "n" 'next-line)
(define-key *jis-code-select-mode-keymap* "l" 'forward-char)
(define-key *jis-code-select-mode-keymap* "p" 'previous-line)
(define-key *jis-code-select-mode-keymap* "q" 'tcode-quit-jis-select)

(define-command tcode-jis-code-select () ()
  (let ((buffer (make-buffer *jis-code-select-buffer-name*
                             :enable-undo-p nil)))
    (unless (buffer-value buffer 'initialized nil)
      (insert-string (buffer-start-point buffer) (make-jis-code-table))
      (setf (buffer-point buffer) (buffer-start-point buffer))
      (setf (buffer-read-only-p buffer) t)
      (setf (buffer-value buffer 'initialized) t)
      (change-buffer-mode buffer 'tc-jis-code-select-mode))
    (pop-to-buffer buffer)
    (setq *target-buffer* (current-buffer))
    (other-window)))

(define-command do-nothing () ()
  ;; do nothing
  )