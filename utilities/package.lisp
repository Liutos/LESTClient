(in-package :common-lisp)

(defpackage #:com.liutos.fw
  (:use #:cl
        #:hunchentoot
        #:json)
  (:shadow #:define-easy-handler
           #:start
           #:stop)
  (:export #:*application*
           #:init
           #:print-routes
           #:render
           #:start
           #:stop))
