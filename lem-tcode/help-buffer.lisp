(uiop/package:define-package :lem-tcode/help-buffer
  (:use :cl :lem)
  (:export :display-help-buffer :remove-help-buffer))

(in-package :lem-tcode/help-buffer)

(defvar *help-buffer-name* "*T-Code Help*")

(defun display-help-buffer (content &optional buffer-name)
  "ヘルプバッファ(default *help-buffer-name*)に content の内容を表示する。"
  (let* ((name (or buffer-name *help-buffer-name*))
         (buffer (make-buffer name)))
    (erase-buffer buffer)
    (insert-string (buffer-start-point buffer) content)
    (pop-to-buffer buffer)))

(defun remove-help-buffer (&optional buffer-name)
  (alexandria:if-let (win (car (get-buffer-windows
                                (get-buffer (or buffer-name
                                                *help-buffer-name*)))))
    (delete-window win)))
