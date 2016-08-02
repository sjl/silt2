(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "vendor/quickutils.lisp"
  :utilities '(
               :with-gensyms
               :once-only
               :compose
               :curry
               :rcurry
               :parse-body
               ; :n-grams
               :define-constant
               ; :switch
               ; :while
               ; :ensure-boolean
               ; :iota
               ; :zip
               )
  :package "SILT.QUICKUTILS")
