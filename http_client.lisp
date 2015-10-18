(in-package #:lestclient)

(defun http-request (url &rest args)
  (multiple-value-bind (body code headers)
      (apply #'drakma:http-request url args)
    (declare (ignorable code))
    (let ((body (if (typep body '(simple-array (unsigned-byte 8)))
                    (flexi-streams:octets-to-string
                     body
                     :external-format :utf-8)
                    body)))
      (values body code headers))))
