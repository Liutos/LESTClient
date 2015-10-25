;;;; lestclient.asd

(asdf:defsystem #:lestclient
  :serial t
  :description "Simulation of RESTClient, a Firefox plugin."
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-annot
               #:cl-json
               #:cl-ppcre
               #:drakma
               #:hunchentoot
               #:local-time
               #:md5
               #:metabang-bind)
  :serial t
  :components ((:module "utilities"
                        :serial t
                        :components ((:file "package")
                                     (:file "ll")
                                     (:file "fw")))
               (:file "package")
               (:file "http_client")
               (:file "lestclient")))

