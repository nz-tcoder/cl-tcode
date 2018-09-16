(in-package :cl-tcode)

(defun char-to-string (ch)
  (format nil "~c" ch))

(defun string-to-char (str)
  (char str 0))

(defun 7bit-ascii-char-p (ch)
  (and (characterp ch)
       (< (char-code ch) 128)))

(defun tcode-string-width (str)
  (reduce #'(lambda (x y)
              (+ x (if (7bit-ascii-char-p y) 1 2)))
          str
          :initial-value 0))

(defun wj (stream x colon-p at-sign-p &rest params)
  "direcitive function for wide character justification.
padding spaces if needed."
  (declare (ignorable colon-p))
  (if (null params)
      (format stream "~a" x)
      (let ((mincol (car params))
            (len (tcode-string-width x)))
        (if (>= len mincol)
            (format stream "~a" x)
            (if at-sign-p
                ;; left alligned
                (format stream "~a~v<~c~>" x (- mincol len) #\space)
                ;; right alliened
                (format stream "~v<~c~>~a" (- mincol len) #\space x))))))
