;;;; lestclient.lisp

(in-package #:lestclient)

;;; "lestclient" goes here. Hacks and glory await!

(enable-annot-syntax)

(defvar *project-dir*
  (asdf:system-source-directory '#:lestclient))

(defvar *static*
  (merge-pathnames "static/" *project-dir*))

(defvar *www*
  (merge-pathnames "html/" *static*))

(defvar *code-ok* 1000)

(defparameter *builtin-headers*
  '(("User-Agent" . :user-agent)))

(defmacro mapeach (list pair-var &body body)
  `(mapcar (lambda (,pair-var)
             ,@body)
           ,list))

(defun headers->alist (headers)
  (unless headers
    (setf headers "{}"))
  (let* ((*json-identifier-name-to-lisp* 'string-capitalize)
         (ht (decode-json-from-string headers))
         (ah '())                       ; Additional Headers
         (kh '()))                      ; Keyword Headers
    (dolist (pair ht)
      (destructuring-bind (key . val) pair
        (let ((it (assoc key *builtin-headers* :test #'string=)))
          (if it
              (progn
                (push val kh)
                (push (cdr it) kh))
              (push (cons (symbol-name key) (lambda () val)) ah)))))
    (values ah kh)))

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

(defun handle-json-response (code headers content)
  (let ((resp
         `((code . ,code)
           (data . ((body . ,content)
                    (headers . ,(headers-capitalize headers)))))))
    (render :json resp)))

(defun method-pp (method)
  (let ((m (if (string= method "")
               "GET"
               method)))
    (intern (string-upcase m) :keyword)))

(define-handler :post "/api/request" handle-api-request (method url headers params)
  (declare (ignorable headers method params))
  (bind (((:values ah kh) (headers->alist headers))
         (method (method-pp method))
         (ps (params->alist params)))
    (multiple-value-bind (body code headers)
        (apply #'http-request
               url
               `(,@kh :additional-headers ,ah :method ,method :parameters ,ps))
      (declare (ignorable body code))
      (handle-json-response *code-ok* headers body))))

;;; Public

@export
(defun lest-start ()
  (setf *application*
        (init
         :access-log-destination "access.log"
         :document-root *www*
         :port 4242
         :static-path "/static/"
         :static-root *static*))
  (start))

@export
(defun lest-stop ()
  (stop))
