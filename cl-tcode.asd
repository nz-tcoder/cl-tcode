(defsystem "cl-tcode"
  :serial t
  :depends-on (:alexandria :cl-store)
  :components ((:file "packages")
               (:file "tc")
               (:file "util")
               (:file "tc-help")
               (:file "dic")
               (:file "cl-tcode")))

