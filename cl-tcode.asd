(defsystem "cl-tcode"
  :serial t
  :depends-on (:lem)
  :components ((:file "tc")
               (:file "util")
               (:file "dic")
               (:file "mazegaki")))
