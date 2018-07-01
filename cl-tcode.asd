(defsystem "cl-tcode"
  :serial t
  :depends-on (:lem)
  :components ((:file "tc")
               (:file "util")
               (:file "tc-help")
               (:file "dic")
               (:file "mazegaki")))
