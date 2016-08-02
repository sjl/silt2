(asdf:defsystem #:silt
  :name "silt"
  :description "Lisp Game Jam, August 2016"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:iterate
               #:cl-charms
               #:cl-arrows)

  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")
                 (:file "state-machine")))
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "main")))))
