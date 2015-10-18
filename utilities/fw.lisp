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

;;; Private functions
(defun handler-name (name)
  (intern (format nil "HANDLER/~A" name)))

(defun parse-description (description)
  (etypecase description
    (cons (destructuring-bind (name . args) description
            (values name
                    (cons (handler-name name)
                          args))))
    (symbol (values description
                    (handler-name description)))))

(defun parse-lambda-list (lambda-list)
  (mapcar #'(lambda (ele)
              (etypecase ele
                (cons (first ele))
                (symbol ele)))
          lambda-list))

;;; Public functions
(defmacro define-easy-handler (description lambda-list &body body)
  (multiple-value-bind (name description)
      (parse-description description)
    (let ((args (parse-lambda-list lambda-list)))
      `(progn
         (defun ,name ,args ,@body)
         (hunchentoot:define-easy-handler ,description ,lambda-list
           (,name ,@args))))))

(defun render (&key
                 file
                 html
                 json
                 nothing
                 plain)
  (when (and file
             html
             json
             nothing
             plain)
    (error "Only one valid argument allowed in ~{`~(~A~)'~^, ~}"
           '(file html json nothing plain)))
  (let (content-type body)
    (cond (file
           (setf body (handle-static-file file)))
          (html
           (setf content-type "text/html"
                 body html))
          (json
           (setf content-type "application/json"
                 body (encode-json-to-string json)))
          (nothing
           (setf body ""))
          (plain
           (setf content-type "text/plain"
                 body plain)))
    (setf (content-type*) content-type)
    (return-from render body)))

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
