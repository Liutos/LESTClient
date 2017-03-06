;;;; lestclient.lisp

(in-package #:lestclient)

(defun home (request)
  "响应首页内容"
  (declare (ignorable request))
  (eloquent.mvc.response:respond
   #P"/home/liutos/src/cl/LESTClient/static/html/index.html"))

(defun api-request (request)
  "请求目标接口"
  (let ((form (eloquent.mvc.request:getextra :body request)))
    (let ((url (eloquent.mvc.prelude:string-assoc "url" form)))
      (eloquent.mvc.response:respond
       (drakma:http-request url)))))
