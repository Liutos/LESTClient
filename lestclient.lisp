;;;; lestclient.lisp

(in-package #:lestclient)

;;; "lestclient" goes here. Hacks and glory await!

(defvar *project-dir*
  (asdf:system-source-directory '#:lestclient))

(defvar *www*
  (merge-pathnames "static/html/" *project-dir*))

(defvar *acceptor*
  (make-instance 'easy-acceptor
                 :document-root *www*
                 :port 4242))

(defvar *code-ok* 1000)

(defun lest-start ()
  (start *acceptor*))

(defun lest-stop ()
  (stop *acceptor*))

(define-easy-handler (say-yo :uri "/yo")
    (name)
  (setf (content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defun handle-static-uri (req)
  (let ((uri (request-uri req)))
    (scan "/static" uri)))

(defun extend-static-uri (uri)
  (merge-pathnames (subseq uri 1) *project-dir*))

(define-easy-handler (handle-static :uri #'handle-static-uri)
    ()
  (let ((path (extend-static-uri (request-uri*))))
    (unless (probe-file path)
      (setf path (merge-pathnames "404.html" *www*)))
    (with-open-file (s path)
      (let ((data (make-string (file-length s))))
        (read-sequence data s)
        data))))

(defparameter *builtin-headers*
  '(user-agent))

(defmacro mapeach (list pair-var &body body)
  `(mapcar (lambda (,pair-var)
             ,@body)
           ,list))

(defun headers->alist (headers)
  (unless headers
    (setf headers "{}"))
  (let* ((*json-identifier-name-to-lisp* 'string-capitalize)
         (ht (decode-json-from-string headers)))
    (mapeach ht pair
      (destructuring-bind (key . val) pair
        (cons (symbol-name key) (lambda () val))))))

(defun type-integer-pp (val)
  (let ((res (register-groups-bind (s e)
                 ("([0-9]+) *~ *([0-9]+)" val)
               (let ((s (parse-integer s))
                     (e (parse-integer e)))
                 (+ (random (- e s)) s)))))
    (if res
        (format nil "~D" res)
        val)))

(defun type-timestamp-pp (val)
  (cond ((string-equal val "now")
         (format nil "~D"
                 (timestamp-to-unix (now))))
        (t val)))

(defun param-value-pp (param-value)
  (destructuring-bind (type val) param-value
    (declare (ignorable type))
    (let ((type (intern (string-upcase type) :keyword)))
      (cond ((eq type :integer)
             (type-integer-pp val))
            ((eq type :timestamp)
             (type-timestamp-pp val))
            (t val)))))

(defun params->alist (params)
  (unless params
    (setf params "{}"))
  (let* ((*json-identifier-name-to-lisp* 'identity)
         (ht (decode-json-from-string params)))
    (mapeach ht pair
      (destructuring-bind (key . val) pair
        (cons (symbol-name key)
              (param-value-pp val))))))

(defun headers-capitalize (headers)
  (mapeach headers cons
    (destructuring-bind (key . val) cons
      (cons (string-capitalize (symbol-name key))
            val))))

(defun handle-response (code headers content)
  (let ((resp
         `((code . ,code)
           (data . ((body . ,content)
                    (headers . ,(headers-capitalize headers)))))))
    (encode-json-to-string resp)))

(defun handle-json-response (code headers content)
  (setf (content-type*) "application/json")
  (let ((*lisp-identifier-name-to-json* 'identity))
    (handle-response code headers content)))

(defun lest-http-request (url &rest args)
  (multiple-value-bind (body code headers)
      (apply #'drakma:http-request url args)
    (declare (ignorable code))
    (let ((body (if (typep body '(simple-array (unsigned-byte 8)))
                    (flexi-streams:octets-to-string
                     body
                     :external-format :utf-8)
                    body)))
      (values body code headers))))

(defun handle-api-request/impl (headers method params url)
  (declare (ignorable headers method params))
  (let ((ap (headers->alist headers))
        (method (intern (string-upcase method) :keyword))
        (ps (params->alist params)))
    (multiple-value-bind (body code headers)
        (lest-http-request
         url
         :additional-headers ap
         :method method
         :parameters ps)
      (declare (ignorable body code))
      (handle-json-response *code-ok* headers body))))

(define-easy-handler (handle-api-request :uri "/api/request")
    (method url headers params)
  (handle-api-request/impl headers method params url))
