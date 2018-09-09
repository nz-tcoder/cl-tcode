(defpackage :cl-tcode
  (:use :cl)
  (:export :tc-mode :wj :char-to-string :string-to-char :clear-strokes
           :tcode-char-to-key
           :toggle-alnum :summarize-by-n
           :+ASCII-SPACE+ :+ASCII-TILDE+ :*tc-engine* :*tcode-char-list*
           :tcode-decode-chars :filter :get-table-size :tcode-draw-table
           :make-stroke-help :show-stroke
           :setup-special-command
           :lookup-mazegaki-dic))
           
           

