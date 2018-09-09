(in-package :cl-tcode)

(defvar *base* (asdf:system-source-directory (asdf:find-system :cl-tcode)))
(defvar *dic-zip-path* (merge-pathnames "master.zip" *base*))

(defvar *pubdic-uri* "https://github.com/nz-tcoder/pubdic-plus-wnn/archive/master.zip")
(defvar *pubdic-path*
  (merge-pathnames (make-pathname :directory '(:relative "pubdic-plus-wnn-master"))
                   *base*))

(defvar *dic-file-list* '("tcode.u" "tankan.u" "kihon.u"
			  "bio.u" "chimei.u" "computer.u" "jinmei.u"
			  "koyuu.u" "setsuji.u" "symbol.u"))

(defvar *mazegakipath* (merge-pathnames "mazegaki-dic.store" *base*))

(defun mazegaki (&key force)
  (or (and (not force) 
           (probe-file *mazegakipath*))
      (and (ql:quickload :dexador :silent t)
           (uiop:symbol-call :dex :fetch *pubdic-uri* *dic-zip-path*
                             :if-exists :overwrite)
           (ql:quickload :zip :silent t)
           (not (uiop:symbol-call :zip :unzip *dic-zip-path* *base*
                                  :if-exists :supersede))
           (make-mazegaki-dic *dic-file-list* *pubdic-path* *mazegakipath*))))

(set-mazegaki-dic (mazegaki))

(setup-tcode (merge-pathnames "tc-tbl.lisp" 
                              (asdf:system-source-directory
                               (asdf:find-system :cl-tcode))))