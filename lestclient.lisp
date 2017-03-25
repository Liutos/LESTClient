;;;; lestclient.lisp

(in-package #:lestclient)

(defun fetch-access-token (client-id client-secret code)
  "Fetch access token for GitHub account by CODE."
  (check-type code string)
  (let* ((raw (drakma:http-request "https://github.com/login/oauth/access_token"
                                   :method :post
                                   :parameters `(("client_id" . ,client-id)
                                                 ("client_secret" . ,client-secret)
                                                 ("code" . ,code))))
         (response (flexi-streams:octets-to-string raw :external-format :utf8)))
    (let ((alist (eloquent.mvc.prelude:parse-query-string response)))
      (declare (ignorable alist))
      (eloquent.mvc.prelude:string-assoc "access_token" alist))))

(defun fetch-user (access-token)
  "Fetch user's information according to ACCESS-TOKEN."
  (check-type access-token string)
  (let* ((raw (drakma:http-request "https://api.github.com/user"
                                   :parameters `(("access_token" . ,access-token))))
         (response (flexi-streams:octets-to-string raw :external-format :utf8)))
    (let* ((cl-json:*identifier-name-to-key* #'identity)
           (cl-json:*json-identifier-name-to-lisp* #'identity)
           (user (cl-json:decode-json-from-string response)))
      user)))

(defun pairs-to-alist (pairs)
  "Converts PAIRS to the form required by :ADDITIONAL-HEADERS and :PARAMETERS in DRAKMA:HTTP-REQUEST"
  (mapcar #'(lambda (pair)
              (cons (cdr (first pair))
                    (cdr (second pair))))
          pairs))

(defun save-user (id user)
  "Save USER owned ID to database."
  (redis:with-connection (:host "127.0.0.1" :port 6381)
    (apply #'red:hmset id
           (alexandria:mappend #'(lambda (pair)
                                   (list (car pair) (cdr pair)))
                               user))))

;;; EXPORT

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
    (handler-case
        (multiple-value-bind (body status-code headers)
            (drakma:http-request url
                                 :additional-headers (pairs-to-alist header)
                                 :connection-timeout timeout
                                 :content body
                                 :external-format-out :utf8
                                 :method (eloquent.mvc.prelude:make-keyword method)
                                 :parameters (pairs-to-alist qs))
          (declare (ignorable status-code))
          (when (typep body '(simple-array (unsigned-byte 8)))
            (setf body
                  (flexi-streams:octets-to-string body :external-format :utf8)))
          (eloquent.mvc.response:respond-json
           `(("data" . (("content" . ,body)
                        ("headers" . ,(mapcar #'(lambda (header)
                                                  `(("field" . ,(car header))
                                                    ("value" . ,(cdr header))))
                                              headers))))
             ("success" . t))))
      (usocket:timeout-error (e)
        (eloquent.mvc.response:respond-json
         `(("error" . ,(format nil "~S" e))
           ("success" . nil)))))))

(defun get-client-id ()
  "Get the client ID for OAuth"
  (let ((config eloquent.mvc.config:*config*))
    (eloquent.mvc.response:respond-json
     `(("data" . (("client-id" . ,(eloquent.mvc.config:get config "OAuth" "client-id"))))
       ("success" . t)))))

(defun home (request)
  "响应首页内容"
  (declare (ignorable request))
  (let* ((config eloquent.mvc.config:*config*)
         (root (eloquent.mvc.config:get-application-root config))
         (home-page (merge-pathnames "static/html/index.html" root)))
    (eloquent.mvc.response:respond home-page)))

(defun sign-in (request)
  (eloquent.mvc.controller:query-string-bind ((code "code"))
      request
    (let* ((config eloquent.mvc.config:*config*)
           (client-id (eloquent.mvc.config:get config "OAuth" "client-id"))
           (client-secret (eloquent.mvc.config:get config "OAuth" "client-secret"))
           (access-token (fetch-access-token client-id client-secret code))
           (user (fetch-user access-token)))
      (save-user (eloquent.mvc.prelude:string-assoc "id" user) user)
      (eloquent.mvc.response:respond
       ""
       :headers (list :location
                      (format nil "http://localhost:8087/?name=~A"
                              (eloquent.mvc.prelude:string-assoc "name" user)))
       :status 302))))

(defun sleepy (request)
  "5秒后再响应"
  (declare (ignorable request))
  (sleep 5)
  (eloquent.mvc.response:respond "OK"))
