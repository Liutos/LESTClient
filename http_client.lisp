(in-package #:lestclient)

(defun http-request (uri &rest args)
  (multiple-value-bind (body code headers)
      (apply #'drakma:http-request uri args)
    (let ((body (if (typep body '(vector (unsigned-byte 8)))
                    (flexi-streams:octets-to-string
                     body
                     :external-format :utf-8)
                    body)))
      (values body code headers))))
