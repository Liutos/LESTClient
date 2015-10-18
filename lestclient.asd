;;;; lestclient.asd

(asdf:defsystem #:lestclient
  :serial t
  :description "Simulation of RESTClient, a Firefox plugin."
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-annot
               #:cl-json
               #:cl-ppcre
               #:drakma
               #:hunchentoot
               #:local-time)
  :components ((:module "utilities"
                        :components ((:file "package")
                                     (:file "fw")))
               (:file "package")
               (:file "lestclient")))

