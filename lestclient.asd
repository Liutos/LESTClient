;;;; lestclient.asd

(asdf:defsystem #:lestclient
  :serial t
  :description "Simulation of RESTClient, a Firefox plugin."
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-json
               #:cl-ppcre
               #:drakma
               #:hunchentoot)
  :components ((:file "package")
               (:file "lestclient")))

