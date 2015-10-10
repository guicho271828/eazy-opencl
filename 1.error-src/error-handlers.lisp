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
  (defun wrap-api (function-info)
    "Return a defun form which wraps the original function, inlining the given cffi function."
    (match function-info
      ((list* _ _ _ args)
       (if (equal '(:pointer error-code) (second (lastcar args)))
           (wrap-api-taking-error-ptr function-info)
           (trivia.skip:skip)))
      ((list* _ _ 'error-code _)
       (wrap-api-returning-error function-info))
      (_
       (wrap-api-normal function-info))))
  
  (defun wrap-api-normal (function-info &optional (wrapper #'identity))
    (match function-info
      ((list* _ (list _ (and lname (symbol name))) _ args)
       (let ((new-args (mapcar (compose #'car #'ensure-list) args))
             (new-name (intern name)))
         `(progn
            (export ',new-name)
            (declaim (inline ,new-name))
            (defun ,new-name ,new-args
              (declare (inline ,lname))
              ,(funcall wrapper `(,lname ,@new-args)))
            (declaim (notinline ,new-name)))))))
  
  (defun wrap-api-returning-error (function-info)
    "Wraps a form when its return
type is a EAZY-OPENCL.BINDING:ERROR-CODE. The code signals an error when
the result is not a :SUCCESS. The error is signalled under the environment
where :RETRY and CL:CONTINUE restart is in effect.

:RETRY will rerun the call to the underlying cffi function.
CONTINUE will ignore the error, then return the result."
    (with-gensyms (result)
      (wrap-api-normal
       function-info
       (lambda (form)
         `(block nil
            (tagbody
              :start
              (restart-case
                  (let ((,result ,form))
                    (unless (eq ,result :success)
                      (error 'opencl-error
                             :code ,result
                             :form ',form)))
                (:retry () (go :start))
                (continue () (return ,result)))))))))

  (defun wrap-api-taking-error-ptr (function-info)
    "Wraps a form when it takes a dynamic pointer to
EAZY-OPENCL.BINDING:ERROR-CODE in its argument.
The code signals an error when the result is not a :SUCCESS. The error is
signalled under the environment where :RETRY and CL:CONTINUE restart is
in effect.

:RETRY will rerun the call to the underlying cffi function.
CONTINUE will ignore the error, then return the result."
    (with-gensyms (result e e-keyword)
      (wrap-api-normal
       (butlast function-info)
       (lambda (form)
         `(block nil
            (tagbody
              :start
              (restart-case
                  (with-foreign-object (,e 'error-code)
                    (let* ((,result (,@form ,e))
                           (,e-keyword (foreign-enum-keyword 'error-code ,e)))
                      (if (eq ,e-keyword :success)
                          ,result
                          (error 'opencl-error
                                 :code ,e-keyword
                                 :form ',form))))
                (:retry () (go :start))
                (continue () (return ,result))))))))))

;; this should be implemented much later... when functional style is finished
;; (defun wrap-resource-manager (function-info form)
;;   (match function-info
;;     ((list* _ (list _ 'create-context) _ args)
;;             ))
;;     (_ form))


(defmacro wrap-apis ()
  `(progn ,@(mapcar #'wrap-api *defined-opencl-functions*)))

(wrap-apis)

