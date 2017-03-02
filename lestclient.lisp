;;;; lestclient.lisp

(in-package #:lestclient)

(defun home (request)
  "响应首页内容"
  (declare (ignorable request))
  (eloquent.mvc.response:respond
   #P"/home/liutos/src/cl/LESTClient/static/html/index.html"))
