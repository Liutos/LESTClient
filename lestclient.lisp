;;;; lestclient.lisp

(in-package #:lestclient)

;;; "lestclient" goes here. Hacks and glory await!

(enable-annot-syntax)

(defvar *project-dir*
  (asdf:system-source-directory '#:lestclient))

(defvar *www*
  (merge-pathnames "static/html/" *project-dir*))

(defvar *acceptor*
  (make-instance 'easy-acceptor
                 :document-root *www*
                 :port 4242))

(defvar *code-ok* 1000)

@export
(defun lest-start ()
  (start *acceptor*))

@export
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

(defun string->format (fmt)
  (declare (ignorable fmt))
  (let ((nfmt fmt))
    (setf nfmt (regex-replace "yyyy" nfmt ",:year,"))
    (setf nfmt (regex-replace "mm" nfmt ",:month,"))
    (setf nfmt (regex-replace "dd" nfmt ",:day,"))
    (setf nfmt (regex-replace "HH" nfmt ",:hour,"))
    (setf nfmt (regex-replace "MM" nfmt ",:min,"))
    (setf nfmt (regex-replace "SS" nfmt ",:sec,"))
    (mapcan #'(lambda (e)
                (cond ((string-equal e "") '())
                      ((string-equal e ":year")
                       (list '(:year 4)))
                      ((and (> (length e) 1)
                            (char= #\: (char e 0)))
                       (list (list (intern (string-upcase (subseq e 1))
                                           :keyword)
                                   2)))
                      (t (list e))))
            (split "," nfmt))))

(defun format-ts (ts fmt)
  (local-time:format-timestring
   nil (local-time:unix-to-timestamp ts)
   :format (string->format fmt)))

(defun type-timestamp-pp (val &aux (pat "(now|[0-9]+)( by (.*))?$"))
  (cond ((string-equal val "now")
         (format nil "~D" (timestamp-to-unix (now))))
        ((when (scan pat val)
           (register-groups-bind (ts _ fmt) (pat val)
             (declare (ignorable _))
             (let ((ts (if (string-equal ts "now")
                           (timestamp-to-unix (now))
                           (parse-integer val :junk-allowed t))))
               (format-ts ts fmt)))))
        (t val)))

(defun md5hex (str)
  (declare (type string str))
  (with-input-from-string (s str)
    (let ((seq (md5:md5sum-stream s)))
      (with-output-to-string (s)
        (dotimes (i (length seq))
          (format s "~2,'0X" (elt seq i)))))))

(defun get-from-result (var result)
  (let* ((name (subseq var (1+ (position #\$ var))))
         (pair (assoc name result :test #'equal)))
    (if pair (cdr pair) var)))

(defun dollar-eval (src result)
  (declare (ignorable result))
  (let ((parts (split " +\\+\\+ +" src)))
    (let ((expr (mapcan
                 (lambda (p)
                   (cond ((char= #\$ (char p 0))
                          (list (get-from-result p result)))
                         ((char= #\" (char p 0))
                          (list (subseq p 1 (- (length p) 2))))
                         (t (list p))))
                 parts)))
      (eval `(concatenate 'string ,@expr)))))

(defun type-sign-pp (val result &aux (pat "(.*) +of +(.*)"))
  (declare (ignorable result))
  (let ((res (register-groups-bind (op src)
                 (pat val)
               (setf src (dollar-eval src result))
               (cond ((string-equal op "md5")
                      (md5hex src))
                     (t src)))))
    (if res res val)))

(defun param-value-pp (param-value result)
  (declare (ignorable result))
  (destructuring-bind (type val) param-value
    (declare (ignorable type))
    (let ((type (intern (string-upcase type) :keyword)))
      (cond ((eq type :integer)
             (type-integer-pp val))
            ((eq type :timestamp)
             (type-timestamp-pp val))
            ((eq type :sign)
             (type-sign-pp val result))
            (t val)))))

(defun params->alist (params)
  (unless params
    (setf params "{}"))
  (let* ((*json-identifier-name-to-lisp* 'identity)
         (ht (decode-json-from-string params)))
    (labels ((aux (ht res)
               (when (null ht)
                 (return-from aux res))
               (destructuring-bind (key . val) (car ht)
                 (push (cons (symbol-name key)
                             (param-value-pp val res))
                       res)
                 (aux (cdr ht) res))))
      (aux ht '()))))

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
