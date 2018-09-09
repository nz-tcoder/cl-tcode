(defsystem "lem-tcode"
  :class :package-inferred-system
  :depends-on ("cl-tcode" "lem"
               "lem-tcode/help-buffer"
               "lem-tcode/mazegaki"
               "lem-tcode/jis-code-select")
  :components
  ((:file "main")))

