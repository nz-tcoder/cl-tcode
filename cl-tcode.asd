(defsystem "cl-tcode"
  :serial t
  :depends-on (:lem :cl-store)
  :components ((:file "packages")
               (:file "tc")
               (:file "util")
               (:file "tc-help")
               (:file "dic")
               (:file "mazegaki")
               (:file "make-dic")))
