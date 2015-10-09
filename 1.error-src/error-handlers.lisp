(in-package :eazy-opencl.error)

;; blah blah blah.

(define-condition opencl-error (error)
  ((code :accessor code :initarg :code :initform (error 'simple-program-error :format-control "~a is a mandatory slot" :code))
   (form :accessor form :initarg :form :initform (error 'simple-program-error :format-control "~a is a mandatory slot" :form)))
  (:report (lambda (c s)
             (match c
               ((opencl-error code form)
                (format s "OpenCL error ~s from ~s" code form))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun wrap-opencl-function (opencl-function-info)
    "Return a defun form which wraps the original function, inlining the given cffi function."
    (match opencl-function-info
      ((list* _ (list _ (and lname (symbol name))) _ args)
       (let ((new-args (mapcar (compose #'car #'ensure-list) args))
             (new-name (intern name)))
         `(progn
            (export ',new-name)
            (declaim (inline ,new-name))
            (defun ,new-name ,new-args
              (declare (inline ,lname))
              ,(wrap-opencl-error-handler
                opencl-function-info
                `(,lname ,@new-args)))
            (declaim (notinline ,new-name)))))))

  (defun wrap-opencl-error-handler (opencl-function-info form)
    "Return a defun form which wraps the original function when its return
type is a EAZY-OPENCL.BINDING:ERROR-CODE. The code signals an error when
the result is not a :SUCCESS. The error is signalled under the environment
where :RETRY and CL:CONTINUE restart is in effect.

:RETRY will rerun the call to the underlying cffi function.
CONTINUE will ignore the error, then return the result."
    (match opencl-function-info
      ((list* _ (list _ (and lname (symbol name))) 'error-code args)
       (let ((new-args (mapcar (compose #'car #'ensure-list) args))
             (new-name (intern name)))
         (with-gensyms (result)
           `(tagbody :start
                     (restart-case
                         (let ((,result ,form))
                           (unless (eq ,result :success)
                             (error 'opencl-error
                                    :code ,result
                                    :form (list ,new-name ,@new-args))))
                       (:retry () (go :start))
                       (continue () (return-from ,new-name ,result)))))))
      (_ form)))

  ;; this should be implemented much later... when functional style is finished
  ;; (defun wrap-resource-manager (opencl-function-info form)
  ;;   (match opencl-function-info
  ;;     ((list* _ (list _ 'create-context) _ args)
  ;;             ))
  ;;     (_ form))
  )

(defmacro wrap-opencl-functions ()
  `(progn ,@(mapcar #'wrap-opencl-function *defined-opencl-functions*)))

(wrap-opencl-functions)

