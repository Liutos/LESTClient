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

;;; Public functions
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
