(asdf:defsystem #:silt
  :name "silt"
  :description "Lisp Game Jam, August 2016"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:iterate
               #:cl-charms
               #+sbcl #:sb-sprof
               #:alexandria
               #:losh
               #:beast)

  :serial t
  :components ((:file "quickutils")
               (:file "package")
               (:file "silt")))
