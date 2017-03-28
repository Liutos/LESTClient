;;;; lestclient.asd

(asdf:defsystem #:lestclient
  :serial t
  :description "Simulation of RESTClient, a Firefox plugin."
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-redis
               #:drakma
               #:eloquent-mvc
               #:uuid)
  :serial t
  :components ((:file "package")
               (:file "http_client")
               (:file "lestclient")))

