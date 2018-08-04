(uiop/package:define-package :lem-tcode/lem (:use :cl :lem))
(in-package :lem-tcode/lem)

(defvar *base* (asdf:system-source-directory (asdf:find-system :lem-tcode)))
(defvar *mazegakiuri* "https://raw.githubusercontent.com/kanchoku/tc/master/tcode/mazegaki.dic")

(defvar *mazegakipath* (merge-pathnames "mazegaki.dic" *base*))

(defun mazegaki (&key force)
  (or (and (not force) 
           (probe-file *mazegakipath*))
      (and (ql:quickload :dexador :silent t)
           (uiop:symbol-call :dex :fetch *mazegakiuri* *mazegakipath* :if-exists :overwrite)
           (probe-file *mazegakipath*))))

(cl-tcode:setup-tcode (merge-pathnames "tc-tbl.lisp" *base*))
(cl-tcode:setup-mazegaki-dic (mazegaki))
