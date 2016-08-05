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
               :hash-table-key-exists-p
               :hash-table-keys
               :hash-table-values
               :map-product
               ; :switch
               ; :while
               ; :ensure-boolean
               ; :iota
               ; :zip
               )
  :package "SILT.QUICKUTILS")
