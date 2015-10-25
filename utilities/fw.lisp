(in-package #:com.liutos.fw)

;;; Utilities
(defmacro setf-default (place new-value)
  `(when (null ,place)
     (setf ,place ,new-value)))

;;; Variable declarations
(defvar *application*)
(defvar *console-output* *standard-output*)
(defvar *routes* '())

;;; Data type definitions
(defclass route-rule ()
  ((handler :initarg :handler
            :reader route-rule-handler)
   (path :initarg :path
         :reader route-rule-path)
   (regex :initarg :regex
          :reader route-rule-regex)
   (verb :initarg :verb
         :reader route-rule-verb)))

(defconstant +PORT+ 4242)

(defclass application (acceptor)
  ((static-path :accessor application-static-path
                :initarg :static-path)
   (static-root :accessor application-static-root
                :initarg :static-root)))

(defun dispatch-handler (request)
  (dolist (rule *routes*)
    (etypecase rule
      (function                         ; Adapt the dispatcher created by `create-folder-dispatcher-and-handler'
       (let ((handler (funcall rule request)))
         (when handler
           (return handler))))
      (route-rule
       (with-slots (handler regex verb) rule
         (when (and (or (eq verb :*) (eq (request-method request) verb)) ; No verb requirement or match
                    (scan regex (script-name request)))
           (return (symbol-function handler))))))))

(defmethod acceptor-dispatch-request ((acceptor application) request)
  (let ((handler (dispatch-handler request)))
    (if handler
        (funcall handler)
        (call-next-method))))

(defclass response ()
  ((body :initarg :body
         :reader response-body)
   (content-type :accessor response-content-type
                 :initarg :content-type)))

(defmethod initialize-instance :after ((instance response) &rest initargs)
  (declare (ignore initargs))
  (setf-default (response-content-type instance) "text/html"))

(defun make-response (body
                      &optional
                        content-type)
  (make-instance 'response
                 :body body
                 :content-type content-type))

;;; Private functions
(defun handler-name (name)
  (intern (format nil "HANDLER/~A" name)))

(defun parse-verb (args)
  (getf args :verb))

(defun parse-uri (args)
  (getf args :uri))

(defun parse-description (description)
  (etypecase description
    (cons (destructuring-bind (name . args) description
            (let* ((verb (parse-verb args))
                   (uri (parse-uri args))
                   (args (if verb
                             (append args '(:allow-other-keys t))
                             args)))
              (values name
                      (cons (handler-name name)
                            args)
                      verb
                      uri))))
    (symbol (values description
                    (handler-name description)))))

(defun parse-lambda-list (lambda-list)
  (let ((args '()))
    (dolist (var lambda-list)
      (when (char= (char (symbol-name var) 0) #\&)
        (return))
      (push var args))
    (nreverse args)))

(defun update-route (verb uri handler)
  (let ((key (format nil "~A ~A" verb uri)))
    (setf (gethash key *routes*) handler)))

(defmacro define-easy-handler-no-verb ((name args body) (description uri lambda-list))
  (let ((response (gensym)))
    `(progn
       ;; Unbind the previous generic function if exists
       (when (and (fboundp ',name)
                  (typep (symbol-function ',name) 'generic-function))
         (fmakunbound ',name))

       (defun ,name ,args ,@body)
       (hunchentoot:define-easy-handler ,description ,lambda-list
         (let ((,response (,name ,@args)))
           (setf (content-type*) (response-content-type ,response))
           (response-body ,response)))
       (update-route nil ,uri ',name))))

(defmacro define-easy-handler-with-verb ((name verb args body) (description uri lambda-list))
  (let ((response (gensym)))
    `(progn
       ;; Unbind the previous ordinary function
       (when (and (fboundp ',name)
                  (not (typep (symbol-function ',name) 'generic-function)))
         (fmakunbound ',name))
       ;; The first time to define the generic function
       (unless (fboundp ',name)
         (defgeneric ,name (verb ,@args)))

       (defmethod ,name ((verb (eql ,verb)) ,@args)
         (declare (ignorable verb))
         ,@body)
       (hunchentoot:define-easy-handler ,description ,lambda-list
         (let ((,response (,name (request-method*) ,@args)))
           (setf (content-type*) (response-content-type ,response))
           (response-body ,response)))
       (update-route ,verb ,uri ',name))))

(defmacro with-io-control (lambda-list expr)
  (let ((response (gensym)))
    `(let ,(mapcar #'(lambda (var)
                       `(,var (hunchentoot::compute-parameter ,(format nil "~(~A~)" var) 'string :both)))
                   lambda-list)
       (let ((,response (let ((*standard-output* *console-output*))
                          ,expr)))
         (cond ((typep ,response 'response)
                (setf (content-type*) (response-content-type ,response))
                (response-body ,response))
               (t ,response))))))

(defparameter *placeholder* ":[^/]+")

(defun parse-vars-and-uri (uri)
  (let ((names (all-matches-as-strings *placeholder* uri))
        (new-uri (regex-replace-all *placeholder* uri "([^/]+)")))
    (values (mapcar #'(lambda (name)
                        (intern (string-upcase (subseq name 1))))
                    names)
            new-uri
            (mapcar #'(lambda (name)
                        (intern (string-upcase (subseq name 1)) :keyword))
                    names))))

(defun double-valid-p (&rest args)
  "Return T if there are more than one no-null value in `args'"
  (let ((cnt 0))
    (dolist (arg args)
      (when arg
        (incf cnt))
      (when (>= cnt 2)
        (return-from double-valid-p t)))
    nil))

(defun ensure-route (verb path uri handler)
  (unless (find-if #'(lambda (rule)
                       (and (typep rule 'route-rule)
                            (eq (route-rule-verb rule) verb)
                            (equal (route-rule-regex rule) uri)
                            (eq (route-rule-handler rule) handler)))
                   *routes*)
    (push (make-instance 'route-rule
                         :handler handler
                         :path path
                         :regex uri
                         :verb verb)
          *routes*)))

;;; Public functions
(defmacro define-easy-handler (description lambda-list &body body)
  (multiple-value-bind (name description verb uri)
      (parse-description description)
    (let ((args (parse-lambda-list lambda-list)))
      (if verb
          `(define-easy-handler-with-verb (,name ,verb ,args ,body) (,description ,uri ,lambda-list))
          `(define-easy-handler-no-verb (,name ,args ,body) (,description ,uri ,lambda-list))))))

(defmacro define-handler (verb path name lambda-list &body body)
  (bind ((args (parse-lambda-list lambda-list))
         (handler (intern (format nil "~A/~A" verb name)))
         ((:values uriargs regex keys) (parse-vars-and-uri path))
         (uri (parse-string regex)))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (defun ,handler ()
         (with-io-control ,args
           ,(if uriargs
                (let ((result (gensym)))
                  `(let (,result)
                     (do-register-groups ,uriargs
                         (,regex (script-name*))
                       (setf ,result
                             (,name ,@args
                                    ,@(mapcan #'list keys uriargs))))
                     ,result))
                `(,name ,@args))))
       (ensure-route ,verb ,path ',uri ',handler))))

(defun md5 (str &key (upper-case-p nil))
  (declare (type string str))
  (let* ((sum (md5sum-string str))
         (hex (with-output-to-string (s)
                (dotimes (i (length sum))
                  (format s "~(~2,'0X~)" (aref sum i))))))
    (if upper-case-p
        (string-upcase hex)
        hex)))

(defun print-routes ()
  (dolist (rule *routes*)
    (when (typep rule 'route-rule)
      (with-slots (handler path regex verb) rule
        (format t "~A ~A => ~A~%" verb path handler)))))

(defun render (&key
                 file
                 html
                 json
                 nothing
                 plain)
  (when (double-valid-p file
                        html
                        json
                        nothing
                        plain)
    (error "Only one valid argument allowed in ~{`~(~A~)'~^, ~}"
           '(file html json nothing plain)))
  (cond (file
         (make-response (handle-static-file file)))
        (html
         (make-response html "text/html"))
        (json
         (let* ((*lisp-identifier-name-to-json* 'identity)
                (str (encode-json-to-string json)))
           (make-response str "application/json")))
        (nothing
         (make-response ""))
        (plain
         (make-response plain "text/plain"))))

(defun init (&key
               (access-log-destination *standard-output*)
               document-root
               (message-log-destination *standard-output*)
               (port +PORT+)
               static-path
               static-root)
  (make-instance 'application
                 :access-log-destination access-log-destination
                 :document-root document-root
                 :message-log-destination message-log-destination
                 :port port
                 :static-path static-path
                 :static-root static-root))

(defun start (&optional (app *application*))
  (with-slots (static-path static-root) app
    (push (create-folder-dispatcher-and-handler
           static-path
           static-root)
          *routes*))
  (hunchentoot:start app))

(defun stop (&optional (app *application*))
  (hunchentoot:stop app))
