(in-package :common-lisp)

(defpackage #:com.liutos.fw
  (:use #:cl
        #:hunchentoot)
  (:shadow #:start
           #:stop)
  (:export #:*application*
           #:init
           #:start
           #:stop))
