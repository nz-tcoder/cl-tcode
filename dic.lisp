(in-package :cl-tcode)

(defvar *tcode-mazegaki-dic* (make-hash-table :test #'equal))

(defstruct word-list
  noun
  conjugation)

(defvar *tcode-dir*)

(defvar *tcode-dic-file-name* "mazegaki.dic")

(defun conjugation-p (word)
  (ppcre:scan "—$" word))

(defun setup-mazegaki-dic (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          do
             (destructuring-bind (yomi kanji)
                 (ppcre:split "\\s+" line)
               (let* ((lst (remove "" (ppcre:split "/" kanji) :test #'equal))
                      (key (if (conjugation-p yomi)
                               (remove #\—  yomi)
                               yomi)))
                 (alexandria:ensure-gethash key *tcode-mazegaki-dic*
                                            (make-word-list))
                 (if (conjugation-p yomi)
                     (setf (word-list-conjugation (gethash key *tcode-mazegaki-dic*))
                           lst)
                     (setf (word-list-noun (gethash key *tcode-mazegaki-dic*))
                           lst)))))))


(defun lookup-mazegaki-dic (yomi &optional with-inflection)
  (let ((wl (gethash yomi *tcode-mazegaki-dic*)))
    (and wl
         (if with-inflection
             (word-list-conjugation wl)
             (word-list-noun wl)))))
  
