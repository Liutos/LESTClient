;;;; lestclient.lisp

(in-package #:lestclient)

(defun create-token ()
  "Creates a token for authorization when requesting /api/request."
  (let ((token (uuid)))
    (red:set token token)
    (red:expire token (* 24 60 60))
    token))

(defun document-to-alist (doc)
  "Convert a DOC of type CL-MONGO:DOCUMENT to a equivalent, serializable alist."
  (check-type doc cl-mongo:document)
  (labels ((aux (doc)
             (cond ((typep doc 'cl-mongo:document)
                    (let ((keys (cl-mongo:get-keys doc)))
                      (mapcar #'(lambda (key)
                                  (cons key
                                        (aux (cl-mongo:get-element key doc))))
                              keys)))
                   (t doc))))
    (let ((id (cl-mongo:doc-id doc)))
      (append (aux doc) (list (cons "_id" id))))))

(defun fetch-access-token (client-id client-secret code)
  "Fetch access token for GitHub account by CODE."
  (check-type code string)
  (let* ((response (http-request "https://github.com/login/oauth/access_token"
                                 :method :post
                                 :parameters `(("client_id" . ,client-id)
                                               ("client_secret" . ,client-secret)
                                               ("code" . ,code)))))
    (let* ((alist (eloquent.mvc.prelude:parse-query-string response))
           (access-token (eloquent.mvc.prelude:string-assoc "access_token" alist)))
      (unless access-token
        (error 'eloquent.mvc.response:http-compatible-error
               :message "获取access_token失败"
               :status 400))
      access-token)))

(defun fetch-user (access-token)
  "Fetch user's information according to ACCESS-TOKEN."
  (check-type access-token string)
  (let* ((response (http-request "https://api.github.com/user"
                                 :parameters `(("access_token" . ,access-token)))))
    (let* ((cl-json:*identifier-name-to-key* #'identity)
           (cl-json:*json-identifier-name-to-lisp* #'identity)
           (user (cl-json:decode-json-from-string response)))
      user)))

(defun pairs-to-kv (pairs)
  "Converts PAIRS to the form returned by CL-MONGO:KV."
  (check-type pairs list)
  (cond ((null pairs)
         (cl-mongo:kv '()))
        ((= (length pairs) 1)
         (let* ((pair (car pairs))
                (key (cdr (first pair)))
                (value (cdr (second pair))))
           (cl-mongo:kv key value)))
        (t
         (cl-mongo:kv (pairs-to-kv (list (car pairs)))
                      (pairs-to-kv (cdr pairs))))))

(defun make-document (body header method qs timeout url user-id)
  "Returns a document can be insert into MongoDB."
  (check-type body string)
  (check-type method string)
  (check-type timeout (or integer null))
  (check-type url string)
  (cl-mongo:kv
   (cl-mongo:kv "body" body)
   (cl-mongo:kv
    (cl-mongo:kv "header" (pairs-to-kv header))
    (cl-mongo:kv
     (cl-mongo:kv "method" method)
     (cl-mongo:kv
      (cl-mongo:kv "qs" (pairs-to-kv qs))
      (cl-mongo:kv
       (cl-mongo:kv "timeout" timeout)
       (cl-mongo:kv
        (cl-mongo:kv "url" url)
        (cl-mongo:kv
         (cl-mongo:kv "_id" (uuid))
         (cl-mongo:kv "user-id" user-id)))))))))

(defun make-headers (headers)
  "Converts field names in HEADERS to capital case style."
  (mapcar #'(lambda (header)
              `(("field" . ,(string-capitalize (car header)))
                ("value" . ,(cdr header))))
          headers))

(defun make-set-cookies (user)
  "Creates a Set-Cookie header for USER."
  (let ((id (eloquent.mvc.prelude:string-assoc "id" user))
        (max-age (* 7 24 60 60))
        (session-id (uuid)))
    (red:set session-id id)
    (red:expire session-id max-age)
    (list :set-cookie (format nil "user-id=~D; Max-Age=~D; HttpOnly" id max-age)
          :set-cookie (format nil "session-id=~A; Max-Age=~D; HttpOnly" session-id max-age))))

(defun pairs-to-alist (pairs)
  "Converts PAIRS to the form required by :ADDITIONAL-HEADERS and :PARAMETERS in HTTP-REQUEST"
  (let ((filtered (remove-if #'(lambda (pair)
                                 (let ((key (cdr (first pair))))
                                   (string= key "")))
                             pairs)))
    (mapcar #'(lambda (pair)
                (cons (cdr (first pair))
                      (cdr (second pair))))
            filtered)))

(defun save-history (body header method qs timeout url user-id)
  "Save the current parameters of request into database."
  (let ((doc (make-document body header method qs timeout url user-id)))
    (cl-mongo:db.insert "request_history" doc)))

(defun save-user (id user)
  "Save USER owned ID to database."
  (apply #'red:hmset id
         (alexandria:mappend #'(lambda (pair)
                                 (list (car pair) (cdr pair)))
                             user)))

(defun uri-to-ip (uri)
  "Lookup the DNS for resolving the IP address of host in URI."
  (let* ((uri (puri:parse-uri uri))
         (host (puri:uri-host uri)))
    (if (cl-ppcre:scan "\\d+\\.\\d+\\.\\d+\\.\\d" host)
        host
        (format nil "~A"  (iolib:lookup-hostname host)))))

(defun uuid ()
  "Generate a uuid in string form."
  (uuid:format-as-urn nil (uuid:make-v4-uuid)))

;;; EXPORT

(defun api-request (request)
  "请求目标接口"
  (eloquent.mvc.controller:json-body-bind
      ((body "body")
       (header "header")
       (method "method" :requirep t)
       (qs "qs")
       (timeout "timeout" :type :integer)
       (token "token")
       (url "url" :requirep t)
       (user-agent "user-agent"))
      request
    (when (zerop (red:del token))
      (error 'eloquent.mvc.response:http-compatible-error
             :message "无效请求"
             :status 403))
    (handler-case
        (let ((ip-address (uri-to-ip url))
              (next-token (create-token))
              (request-before (eloquent.mvc.prelude:now :millisecond))
              request-after)
          (save-history body header method qs timeout url
                        (eloquent.mvc.request:get-cookie request "user-id"))
          (multiple-value-bind (body status-code headers)
              (http-request url
                            :additional-headers (pairs-to-alist header)
                            :connection-timeout (or timeout (values))
                            :content (if (string= body "") nil body)
                            :external-format-out :utf8
                            :method (eloquent.mvc.prelude:make-keyword method)
                            :parameters (pairs-to-alist qs)
                            :user-agent user-agent)
            (setf request-after (eloquent.mvc.prelude:now :millisecond))
            (eloquent.mvc.response:respond-json
             `(("data" . (("content" . ,body)
                          ("headers" . ,(make-headers headers))
                          ("ip-address" . ,ip-address)
                          ("status-code" . ,status-code)
                          ("token" . ,next-token)
                          ("total-time" . ,(- request-after request-before))))
               ("success" . t)))))
      (usocket:timeout-error (e)
        (eloquent.mvc.response:respond-json
         `(("error" . ,(format nil "~S" e))
           ("success" . nil)))))))

(defun check-authentication (request next &key)
  "The request to any API except home page and sign in page must be authenticated."
  (let ((path-info (eloquent.mvc.request:request-path-info request)))
    ;; Check the Cookie HTTP header
    (when (and (string/= path-info "/")
               (string/= path-info "/api/client_id")
               (string/= path-info "/middlewares")
               (string/= path-info "/sign_in"))
      (let ((session-id (eloquent.mvc.request:get-cookie request "session-id"))
            (user-id (eloquent.mvc.request:get-cookie request "user-id")))
        (when (or (null session-id) (null user-id))
          (error 'eloquent.mvc.response:http-compatible-error
                 :message "请先登录"
                 :status 401))
        (let ((user-id-stored (red:get session-id)))
          (when (string/= user-id user-id-stored)
            (error 'eloquent.mvc.response:http-compatible-error
                   :message "请先登录"
                   :status 401)))))
    (funcall next request)))

(defun connect-to-redis (request next &key config)
  "Prepare to connect to redis."
  (redis:with-connection (:port (eloquent.mvc.config:get config "redis" "port"))
    (funcall next request)))

(defun get-client-id ()
  "Get the client ID for OAuth"
  (let ((config eloquent.mvc.config:*config*))
    (eloquent.mvc.response:respond-json
     `(("data" . (("client-id" . ,(eloquent.mvc.config:get config "OAuth" "client-id"))
                  ("sign-in-uri" . ,(eloquent.mvc.config:get config "OAuth" "sign-in-uri"))))
       ("success" . t)))))

(defun get-request-token ()
  "Get a token for future requesting to /api/request."
  (let ((token (create-token)))
    (eloquent.mvc.response:respond-json
     `(("data" . (("token" . ,token)))
       ("success" . t)))))

(defun get-user (request)
  "Get the user's information saved when signed in."
  (let ((user-id (eloquent.mvc.request:get-cookie request "user-id")))
    (let* ((keys (red:hkeys user-id))
           (user (mapcar #'(lambda (key)
                             (let ((value (red:hget user-id key)))
                               (cons key value)))
                         keys)))
      (eloquent.mvc.response:respond-json
       `(("data" . (("user" . ,user)))
         ("success" . t))))))

(defun home (request)
  "响应首页内容"
  (declare (ignorable request))
  (let* ((config eloquent.mvc.config:*config*)
         (root (eloquent.mvc.config:get-application-root config))
         (home-page (merge-pathnames "static/html/index.html" root)))
    (eloquent.mvc.response:respond home-page)))

(defun request-history (request)
  "获取当前用户的请求历史"
  (let ((user-id (eloquent.mvc.request:get-cookie request "user-id")))
    (eloquent.mvc.controller:query-string-bind ((limit "limit" :default 10)
                                                (offset "offset" :default 0))
        request
      (let* ((result (cl-mongo:db.find "request_history"
                                       (cl-mongo:kv "user-id" user-id)
                                       :limit limit
                                       :skip offset))
             (docs (cadr result)))
        (eloquent.mvc.response:respond-json
         (mapcar #'document-to-alist docs))))))

(defun show-middlewares ()
  "展示应用所启用的中间件"
  (let ((cl-json:*lisp-identifier-name-to-json* 'identity)
        (middlewares eloquent.mvc.middleware:*middlewares*))
    (eloquent.mvc.response:respond-json
     middlewares)))

(defun sign-in (request)
  (eloquent.mvc.controller:query-string-bind ((code "code"))
      request
    (let* ((config eloquent.mvc.config:*config*)
           (client-id (eloquent.mvc.config:get config "OAuth" "client-id"))
           (client-secret (eloquent.mvc.config:get config "OAuth" "client-secret"))
           (access-token (fetch-access-token client-id client-secret code))
           (user (fetch-user access-token)))
      (save-user (eloquent.mvc.prelude:string-assoc "id" user) user)
      (let ((set-cookies (make-set-cookies user)))
        (eloquent.mvc.response:respond
         ""
         :headers `(:location
                    ,(format nil "/?name=~A"
                             (eloquent.mvc.prelude:string-assoc "name" user))
                    ,@set-cookies)
         :status 302)))))

(defun sign-out (request)
  "登出"
  (let ((session-id (eloquent.mvc.request:get-cookie request "session-id")))
    (red:del session-id)
    '()))

(defun sleepy (request)
  "5秒后再响应"
  (declare (ignorable request))
  (sleep 5)
  (eloquent.mvc.response:respond "OK"))

(defun start ()
  "启动应用"
  (let ((directory (asdf:system-source-directory :lestclient)))
    (flet ((before-hook (config)
             (let ((db (eloquent.mvc.config:get config "mongo" "db"))
                   (host (eloquent.mvc.config:get config "mongo" "host"))
                   (port (eloquent.mvc.config:get config "mongo" "port")))
               (cl-mongo:mongo :db db
                               :host host
                               :port port)
               (cl-mongo:db.use db))))
      (eloquent.mvc.loader:load directory
                                :before-hook #'before-hook))))

(defun stop ()
  "关闭应用"
  (let ((directory (asdf:system-source-directory :lestclient)))
    (flet ((before-hook ()
             (format t "Goodbye~%")))
      (eloquent.mvc.loader:unload directory
                                  :before-hook #'before-hook))))
