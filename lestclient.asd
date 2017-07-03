;;;; lestclient.asd

(asdf:defsystem #:lestclient
  :serial t
  :description "Simulation of RESTClient, a Firefox plugin."
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-mongo
               #:cl-redis
               #:drakma
               #:eloquent-mvc
               ;; #:iolib
               #:puri
               #:uuid)
  :serial t
  :components ((:file "package")
               (:file "http_client")
               (:file "lestclient")))

