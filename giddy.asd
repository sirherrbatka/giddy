(asdf:defsystem giddy
  :name "giddy"
  :author "Marek Kochanowicz"
  :depends-on (#:cl-data-structures
               #:metabang-bind
               #:lparallel
               #:iterate
               #:alexandria
               #:serapeum)
  :pathname "source"
  :components ((:file "aux-package")
               (:module "protocol"
                :components ((:file "package")
                             (:file "variables")
                             (:file "macros")
                             (:file "generics")
                             (:file "types")
                             (:file "utils")
                             (:file "functions")
                             (:file "methods")))
               (:module "api"
                :components ((:file "package")
                             (:file "types")
                             (:file "methods")))))
