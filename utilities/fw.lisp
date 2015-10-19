(in-package #:com.liutos.fw)

;;; Utilities
(defmacro setf-default (place new-value)
  `(when (null ,place)
     (setf ,place ,new-value)))

;;; Variable declarations
(defvar *application*)

;;; Data type definitions
(defconstant +PORT+ 4242)

(defclass application ()
  ((acceptor :accessor application-acceptor)
   (document-root :accessor application-document-root
                  :initarg :document-root)
   (port :accessor application-port
         :initarg :port
         :initform +PORT+)
   (static-path :accessor application-static-path
                :initarg :static-path)
   (static-root :accessor application-static-root
                :initarg :static-root)))

(defmethod initialize-instance :after ((instance application) &rest initargs)
  (declare (ignore initargs))
  (setf-default (application-port instance) +PORT+))

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

(defun parse-description (description)
  (etypecase description
    (cons (destructuring-bind (name . args) description
            (let* ((verb (parse-verb args))
                   (args (if verb
                             (append args '(:allow-other-keys t))
                             args)))
              (values name
                      (cons (handler-name name)
                            args)
                      (parse-verb args)))))
    (symbol (values description
                    (handler-name description)))))

(defun parse-lambda-list (lambda-list)
  (mapcar #'(lambda (ele)
              (etypecase ele
                (cons (first ele))
                (symbol ele)))
          lambda-list))

(defmacro define-easy-handler-no-verb ((name args body) (description lambda-list))
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
           (response-body ,response))))))

(defmacro define-easy-handler-with-verb ((name verb args body) (description lambda-list))
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
           (response-body ,response))))))

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
  (multiple-value-bind (name description verb)
      (parse-description description)
    (let ((args (parse-lambda-list lambda-list)))
      (if verb
          `(define-easy-handler-with-verb (,name ,verb ,args ,body) (,description ,lambda-list))
          `(define-easy-handler-no-verb (,name ,args ,body) (,description ,lambda-list))))))

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
               document-root
               port
               static-path
               static-root)
  (make-instance 'application
                 :document-root document-root
                 :port port
                 :static-path static-path
                 :static-root static-root))

(defun start (&optional
                (app *application*))
  (let ((acceptor (make-instance 'easy-acceptor
                                 :document-root (application-document-root app)
                                 :port (application-port app))))
    (setf (application-acceptor app) acceptor)
    (push (create-folder-dispatcher-and-handler (application-static-path app)
                                                (application-static-root app))
          *dispatch-table*)
    (hunchentoot:start acceptor)))

(defun stop (&optional
               (app *application*))
  (let ((acceptor (application-acceptor app)))
    (hunchentoot:stop acceptor)))
