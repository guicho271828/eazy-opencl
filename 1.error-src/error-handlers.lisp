(in-package :eazy-opencl.error)

;; blah blah blah.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun wrap-api (function-info)
    "Return a defun form which wraps the original function, inlining the given cffi function."
    (match function-info
      ((list* _ _ args)
       (if (equal '(:pointer error-code) (second (lastcar args)))
           (wrap-api-taking-error-ptr function-info)
           (trivia.skip:skip)))
      (_
       (wrap-api-normal function-info))))

  (defun wrap-api-normal (function-info &optional (wrapper #'identity))
    (match function-info
      ((list* (list _ (and lname (symbol name))) _ args)
       (let ((new-args (mapcar (compose #'car #'ensure-list) args))
             (new-name (intern name)))
         `(progn
            (export ',new-name)
            (declaim (inline ,new-name))
            (defun ,new-name ,new-args
              (declare (inline ,lname))
              ,(funcall wrapper `(,lname ,@new-args)))
            (declaim (notinline ,new-name)))))))

  (defun wrap-api-taking-error-ptr (function-info)
    "Takes a info of a function which takes a pointer to EAZY-OPENCL.BINDING:ERROR-CODE
in its last argument, then return the wrapped code.
It signals an error when the error-code stored in the pointer is not a :SUCCESS."
    (with-gensyms (result e)
      (wrap-api-normal
       (butlast function-info)
       (lambda (form)
         `(with-foreign-object (,e 'error-code)
            (let* ((,result (,@form ,e)))
              (mem-ref ,e 'error-code)
              ,result)))))))

(defmacro wrap-apis ()
  `(progn ,@(mapcar #'wrap-api *defined-opencl-functions*)))

(wrap-apis)

