;;;; package.lisp

(defpackage #:lestclient
  (:use #:cl
        #:cl-annot
        #:json
        #:cl-ppcre
        #:hunchentoot

        #:com.liutos.fw)
  (:import-from #:drakma #:http-request)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix)
  (:shadowing-import-from #:com.liutos.fw
                          #:start
                          #:stop)
  (:export #:lest-start
           #:lest-stop))

