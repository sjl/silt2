(defpackage #:silt.utils
  (:use
    #:cl
    #:iterate
    #:cl-arrows
    #:silt.quickutils)
  (:export
    #:zap%
    #:%
    #:recursively
    #:recur
    #:dis
    #:spit
    #:d
    #:clamp

    #:dlambda

    #:hash-set
    #:make-set
    #:set-contains-p
    #:set-add
    #:set-remove
    #:set-add-all
    #:set-remove-all
    #:set-random
    #:set-pop
    #:set-empty-p
    #:set-clear

    #:averaging
    #:timing
    #:real-time
    #:run-time
    #:since-start-into
    #:per-iteration-into
    #:in-whatever

    )
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:silt
  (:use
    #:cl
    #:iterate
    #:cl-arrows
    #:silt.quickutils
    #:silt.utils))
