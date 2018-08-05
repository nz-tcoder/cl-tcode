(in-package :cl-tcode)

(defvar *tcode-mazegaki-dic*)

(defstruct word-list
  noun
  conjugation)

(defvar *tcode-dir*)

(defun conjugation-p (word)
  (ppcre:scan "—$" word))

(defun set-mazegaki-dic (file/hash)
  (setq *tcode-mazegaki-dic*
        (if (hash-table-p file/hash)
            file/hash
            (cl-store:restore file/hash))))

(defun lookup-mazegaki-dic (yomi &optional with-inflection)
  (let ((wl (gethash yomi *tcode-mazegaki-dic*)))
    (and wl
         (if with-inflection
             (word-list-conjugation wl)
             (word-list-noun wl)))))
  
