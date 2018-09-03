(defsystem "cl-tcode"
  :serial t
  :depends-on (:lem :lem-encodings/euc-jp :cl-store)
  :components ((:file "packages")
               (:file "tc")
               (:file "util")
               (:file "tc-help")
               (:file "jis-code-select")
               (:file "dic")
               (:file "mazegaki")
               (:file "make-dic")))
