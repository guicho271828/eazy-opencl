;; Note: the code here is taken from cl-opencl-3b. I do not understand what
;; they are doing, but it seems like something related to dumping the lisp image.
;; I do not understand the later macros either. What are they for?

(cl:in-package #:eazy-opencl.bindings)

;; deprecated in opencl1.2?
(cffi:defcfun ("clGetExtensionFunctionAddress" get-extension-function-address)
    (:pointer :void)
  (function-name :string))

(cl:eval-when (:load-toplevel :execute)
  #+clisp (cl:pushnew 'reset-cl-pointers custom:*fini-hooks*)
  #+sbcl (cl:pushnew 'reset-cl-pointers sb-ext:*save-hooks*)
  #+cmu (cl:pushnew 'reset-cl-pointers ext:*before-save-initializations*)
  #-(or clisp sbcl cmu)
  (cl:warn "Don't know how to setup a hook before saving cores on this Lisp."))

(cl:defparameter *cl-extension-resetter-list* cl:nil)
;;; FIXME? There's a possible race condition here, but this function
;;; is intended to be called while saving an image, so if someone is
;;; still calling CL functions we lose anyway...
(cl:defun reset-cl-pointers ()
  (cl:format cl:t "~&;; resetting OpenCL extension pointers...~%")
  (cl:mapc #'cl:funcall *cl-extension-resetter-list*)
  (cl:setf *cl-extension-resetter-list* cl:nil))


(cl:defmacro defclfun (name return-type cl:&body args)
  `(cffi:defcfun ,name ,return-type ,@args))
#++
(cl:defmacro defclfun (name return-type cl:&body args)
  (cl:let ((n (cl:gensym (cl:second name))))
    `(cl:progn
       (cffi:defcfun (,(cl:car name) ,n) ,return-type ,@args)
       (cl:defun ,(cl:second name) ,(cl:mapcar 'cl:first args)
         #++(cl:format cl:t "call ~s:~%   ~s~%" ',name
                    (cl:list ,@(cl:loop for (i cl:nil) in args
                                        collect (cl:format cl:nil "~s:" i)
                                        collect i)))
         (,n ,@(cl:mapcar 'cl:first args))
         ))))


(cl:defmacro defclextfun ((cname lname) return-type cl:&body args)
  (alexandria:with-unique-names (pointer)
    `(cl:let ((,pointer (null-pointer)))
       (cl:defun ,lname ,(cl:mapcar #'cl:car args)
         #++(cl:format cl:t "call ext ~s: ~s~%" ',lname (cl:list ,@(cl:mapcar 'cl:first args)))
         (cl:when (null-pointer-p ,pointer)
           (cl:setf ,pointer (get-extension-function-address ,cname))
           (cl:assert (cl:not (null-pointer-p ,pointer)) ()
                   "Couldn't load symbol ~A~%" ,cname)
           (cl:format cl:t "Loaded function pointer for ~A: ~A~%" ,cname ,pointer)
           (cl:push (cl:lambda () (cl:setf ,pointer (null-pointer)))
                 *cl-extension-resetter-list*))
         (foreign-funcall-pointer
          ,pointer
          (:library opencl)
          ,@(cl:loop for arg in args
                     collect (cl:second arg)
                     collect (cl:first arg))
          ,return-type)))))

