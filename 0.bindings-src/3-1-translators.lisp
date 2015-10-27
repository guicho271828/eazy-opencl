;;; translator
(in-package :eazy-opencl.bindings)

;;; bool

(define-foreign-type bool-type ()
  ()
  (:actual-type --bool)
  (:simple-parser bool))

(defmethod translate-to-foreign (lispobj (type bool-type))
  (if lispobj true false))

(defmethod translate-from-foreign (c-obj (type bool-type))
  (= c-obj true))

;;; error-code

(define-foreign-type error-code-type ()
  ()
  (:actual-type --error-code)
  (:simple-parser error-code))

(define-condition opencl-error (error)
  ((code :accessor opencl-error-code
         :initarg :code
         :initform (error 'simple-program-error
                          :format-control "Missing required argument: :CODE")))
  (:report (lambda (c s)
             (with-slots (code) c
                (format s "OpenCL error ~s" code)))))

(defmethod print-object ((c opencl-error) s)
  (print-unreadable-object (c s :type t)
    (with-slots (code) c
       (format s ":code ~s" code))))

(defmethod translate-from-foreign (c-obj (type error-code-type))
  (let ((code (foreign-enum-keyword '--error-code c-obj)))
    (if (eq code :success)
        :success
        (error 'opencl-error :code code))))

;;; OpenCL objects e.g. program, context

(defstruct box
  (value 0 :read-only t))

(defmacro define-cl-object-type (type &key
                                        (actual-type (symbolicate '-- type))
                                        (releaser (symbolicate 'release- type))
                                        (inherit 'box))
  (let* ((translator (symbolicate type '-type))
         (box (symbolicate 'boxed- type))
         (unbox (symbolicate box '-value)))
    `(progn
       (define-foreign-type ,translator ()
         ()
         (:actual-type ,actual-type)
         (:simple-parser ,type))
       (defstruct (,box (:include ,inherit) (:constructor ,box (value))))
       (defmethod translate-to-foreign ((lisp-obj ,box) (type ,translator))
         (,unbox lisp-obj))
       (defmethod translate-from-foreign (c-obj (type ,translator))
         (let ((box (,box c-obj)))
           (finalize box (lambda ()
                           ;; FIXME: this notinline is here because
                           ;; ,realeaser is later declared inline, and sbcl
                           ;; complains it. For a better solution
                           ;; delay the definition of translate-from-foreign.
                           (declare (notinline ,releaser))
                           (,releaser c-obj))))))))

(define-cl-object-type command-queue)
(define-cl-object-type context)
(define-cl-object-type device-id :releaser release-device)
(define-cl-object-type event)
(define-cl-object-type kernel)
(define-cl-object-type mem :releaser release-mem-object)
(define-cl-object-type program)
(define-cl-object-type sampler)

;; TODO: rewrite mem to specialized versions
(define-cl-object-type buffer
    :actual-type mem
    :releaser release-mem-object
    :inherit boxed-mem)
(define-cl-object-type image
    :actual-type mem
    :releaser release-mem-object
    :inherit boxed-mem)
(define-cl-object-type pipe
    :actual-type mem
    :releaser release-mem-object
    :inherit boxed-mem)
(define-cl-object-type sub-buffer
    :actual-type buffer
    :releaser release-mem-object
    :inherit boxed-buffer)
