;;;; lestclient.lisp

(in-package #:lestclient)

(defun home (request)
  "响应首页内容"
  (declare (ignorable request))
  (eloquent.mvc.response:respond
   #P"/home/liutos/src/cl/LESTClient/static/html/index.html"))

(defun api-request (request)
  "请求目标接口"
  (eloquent.mvc.controller:json-body-bind
      ((body "body")
       (header "header")
       (method "method" :requirep t)
       (qs "qs")
       (url "url" :requirep t))
      request
    (format t "header: ~S~%" header)
    (eloquent.mvc.response:respond
     (drakma:http-request url
                          :content body
                          :method (eloquent.mvc.prelude:make-keyword method)
                          :parameters (mapcar #'(lambda (pair)
                                                  (cons (cdar pair)
                                                        (cdadr pair)))
                                              qs)))))
