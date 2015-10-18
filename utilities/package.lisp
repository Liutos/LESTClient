(in-package :common-lisp)

(defpackage #:com.liutos.fw
  (:use #:cl
        #:hunchentoot)
  (:shadow #:define-easy-handler
           #:start
           #:stop)
  (:export #:*application*
           #:init
           #:start
           #:stop))
