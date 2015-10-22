;;;; package.lisp

(defpackage #:lestclient
  (:use #:cl
        #:cl-annot
        #:json
        #:cl-ppcre
        #:hunchentoot
        #:metabang.bind

        #:com.liutos.fw)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix)
  (:shadow #:http-request)
  (:shadowing-import-from #:com.liutos.fw
                          #:define-easy-handler
                          #:start
                          #:stop)
  (:export #:lest-start
           #:lest-stop))

