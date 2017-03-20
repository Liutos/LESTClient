;;;; lestclient.lisp

(in-package #:lestclient)

(defun pairs-to-alist (pairs)
  "Converts PAIRS to the form required by :ADDITIONAL-HEADERS and :PARAMETERS in DRAKMA:HTTP-REQUEST"
  (mapcar #'(lambda (pair)
              (cons (cdr (first pair))
                    (cdr (second pair))))
          pairs))

;;; EXPORT

(defun home (request)
  "响应首页内容"
  (declare (ignorable request))
  (let* ((config eloquent.mvc.config:*config*)
         (root (eloquent.mvc.config:get-application-root config))
         (home-page (merge-pathnames "static/html/index.html" root)))
    (eloquent.mvc.response:respond home-page)))

(defun api-request (request)
  "请求目标接口"
  (eloquent.mvc.controller:json-body-bind
      ((body "body")
       (header "header")
       (method "method" :requirep t)
       (qs "qs")
       (timeout "timeout" :type :integer)
       (url "url" :requirep t))
      request
    (eloquent.mvc.response:respond
     (drakma:http-request url
                          :additional-headers (pairs-to-alist header)
                          :connection-timeout timeout
                          :content body
                          :method (eloquent.mvc.prelude:make-keyword method)
                          :parameters (pairs-to-alist qs)))))

(defun sleepy (request)
  "5秒后再响应"
  (declare (ignorable request))
  (sleep 5)
  (eloquent.mvc.response:respond "OK"))
