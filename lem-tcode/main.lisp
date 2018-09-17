(uiop/package:define-package :lem-tcode
  (:use :cl :lem)
  (:export :tcode-display-help-buffer :*tcode-clear-hook*))

(in-package :lem-tcode)

(defvar *tcode-clear-hook* nil)

(define-command tcode-clear () ()
  "ややこしいモードに入っているのを全部クリアする。
ヘルプ用ウィンドウも消去する。"
  (lem-tcode/help-buffer:remove-help-buffer)
  (lem.popup-window::clear-popup-message)
  (run-hooks *tcode-clear-hook*))

(define-minor-mode tc-mode
    (:name "tc"
     :keymap *tc-mode-keymap*
     :disable-hook 'lem-tcode/mazegaki:clear-help)
  (tcode-clear))

(defun tc-mode-p ()
  (mode-active-p (current-buffer) 'tc-mode))

(define-command tcode-self-insert-command () ()
  (unless (lem-tcode/mazegaki:mazegaki-mode-p)
    (lem-tcode/mazegaki:clear-help))
  (destructuring-bind (decoded &rest args)
      (cl-tcode:tcode-decode-chars cl-tcode:*tc-engine*
                                   (insertion-key-p (last-read-key-sequence)))
    (declare (ignorable args))
    (cond ((consp decoded)
           (apply (car decoded) (cdr decoded)))
          ((characterp decoded)
           (insert-character (current-point)
                             (cl-tcode:filter cl-tcode:*tc-engine* decoded))
           #+tcode-trace
           (message "tcode trace: ~a~%" args)
           )
          ((functionp decoded)
           (funcall decoded))
          ((and (symbolp decoded) (lem::get-command decoded))
           (funcall decoded))
          ((eq decoded t)
           (cl-tcode:clear-strokes cl-tcode:*tc-engine*)))))

(defun cancel-strokes ()
  (when (tc-mode-p)
    (unless (lem::keymap-find-keybind *tc-mode-keymap*
                                      (last-read-key-sequence) nil)
      (cl-tcode:clear-strokes cl-tcode:*tc-engine*))))

(add-hook *post-command-hook* 'cancel-strokes)

(defvar *help-string*
  "Tコードモード中のキー操作は次のとおり。
   22 : JISコード入力
   33 : Tコード表にある英数字・記号の文字コードの半角・全角切り替え。
   fj : 交ぜ書き変換を行う)。
   M-\\: Tコードモードを抜ける。")

(define-command tc-mode-help () ()
  (lem-tcode/help-buffer:display-help-buffer *help-string*))

(define-command toggle-alnum-mode () ()
  (cl-tcode:toggle-alnum cl-tcode:*tc-engine*))

(define-command tc-show-tables (seq) ("sRL,RR,LR,LL: ")
  (alexandria:if-let ((help-table (cl-tcode:make-stroke-help seq)))
                     (lem-tcode/help-buffer:display-help-buffer
                      (format nil "~a~%~%~a" (string-upcase seq) help-table))))

(loop for code from cl-tcode:+ASCII-SPACE+ to cl-tcode:+ASCII-TILDE+
      for char = (code-char code)
      do
         (define-key *tc-mode-keymap*
           (if (eql char #\space)
               "Space"
               (format nil "~c" char)) 'tcode-self-insert-command))
(define-key *tc-mode-keymap* "?" 'tc-mode-help)
(define-key *global-keymap* "M-\\" 'tc-mode)

;;; mazegaki
(add-hook *tcode-clear-hook* 'lem-tcode/mazegaki:mazegaki-finish)

;;; set special command
(defvar *special-commands-alist*
  '(((0 0) . ; 11 :  LL表の表示
     (tc-show-tables "LL")) 
    ((0 9) . ; 10 :  LR表の表示
     (tc-show-tables "LR")) 
    ((9 0) . ; 01 :  RL表の表示
     (tc-show-tables "RL"))
    ((9 9) . ; 00 :  RR表の表示
     (tc-show-tables "RR"))
    ((1 1) . ; 22 : JISコード入力
     lem-tcode/jis-code-select:tcode-jis-code-select)
    ((2 2) . ; 33 : 半角・全角切り替え。
     toggle-alnum-mode)
    ((23 26) . ; fj: 交ぜ書き変換
     lem-tcode/mazegaki:mazegaki-begin-conversion)))

(cl-tcode:setup-special-command cl-tcode:*tc-engine* *special-commands-alist*)
