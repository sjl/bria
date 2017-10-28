(asdf:defsystem :bria
  :description "Basic, Reliable IRC Assistant"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on (:birch
               :bordeaux-threads
               :cl-interpol
               :drakma
               :flexi-streams
               :iterate
               :losh
               :named-readtables
               :yason)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "readtables")
                 (:file "main")))))

