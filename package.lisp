;;;; package.lisp

(defpackage #:lestclient
  (:use #:cl)
  (:export #:connect-to-redis
           #:check-authentication
           #:start
           #:stop))

