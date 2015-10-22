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
(defconstant +PORT+ 4242)

(defclass application (acceptor)
  ((static-path :accessor application-static-path
                :initarg :static-path)
   (static-root :accessor application-static-root
                :initarg :static-root)))

(defun dispatch-handler (request)
  (dolist (rule *routes*)
    (etypecase rule
      (cons
       (destructuring-bind (verb uri name) rule
         (when (and (or (eq verb :*)
                        (eq (request-method request) verb))
                    (string= (script-name request) uri))
           (return (symbol-function name)))))
      (function                         ; Adapt the dispatcher created by `create-folder-dispatcher-and-handler'
       (let ((handler (funcall rule request)))
         (when handler
           (return handler)))))))

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
  (mapcar #'(lambda (ele)
              (etypecase ele
                (cons (first ele))
                (symbol ele)))
          lambda-list))

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

(defun double-valid-p (&rest args)
  "Return T if there are more than one no-null value in `args'"
  (let ((cnt 0))
    (dolist (arg args)
      (when arg
        (incf cnt))
      (when (>= cnt 2)
        (return-from double-valid-p t)))
    nil))

;;; Public functions
(defmacro define-easy-handler (description lambda-list &body body)
  (multiple-value-bind (name description verb uri)
      (parse-description description)
    (let ((args (parse-lambda-list lambda-list)))
      (if verb
          `(define-easy-handler-with-verb (,name ,verb ,args ,body) (,description ,uri ,lambda-list))
          `(define-easy-handler-no-verb (,name ,args ,body) (,description ,uri ,lambda-list))))))

(defmacro define-handler (verb uri name lambda-list &body body)
  (let ((handler (intern (format nil "~A/~A" verb name)))
        (response (gensym)))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (defun ,handler ()
         (let ,(mapcar #'(lambda (var)
                           `(,var (hunchentoot::compute-parameter ,(format nil "~(~A~)" var) 'string :both)))
                       lambda-list)
           (let ((,response (let ((*standard-output* *console-output*))
                              (,name ,@lambda-list))))
             (cond ((typep ,response 'response)
                    (setf (content-type*) (response-content-type ,response))
                    (response-body ,response))
                   (t ,response)))))
       (push '(,verb ,uri ,handler)
             *routes*))))

(defun print-routes ()
  (dolist (rule *routes*)
    (when (consp rule)
      (destructuring-bind (verb uri name) rule
        (format t "~A ~A => ~A~%" verb uri name)))))

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
               access-log-destination
               document-root
               (port +PORT+)
               static-path
               static-root)
  (make-instance 'application
                 :access-log-destination access-log-destination
                 :document-root document-root
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
