;; Note: the code here is taken from cl-opencl-3b. I do not understand what
;; they are doing, but it seems like something related to dumping the lisp image.
;; I do not understand the later macros either. What are they for? --- guicho 2015/10/9

;; ;; deprecated in opencl1.2?
;; (defcfun ("clGetExtensionFunctionAddress" get-extension-function-address)
;;     (:pointer :void)
;;   (function-name :string))
;; 
;; (eval-when (:load-toplevel :execute)
;;   #+clisp (pushnew 'reset-cl-pointers custom:*fini-hooks*)
;;   #+sbcl (pushnew 'reset-cl-pointers sb-ext:*save-hooks*)
;;   #+cmu (pushnew 'reset-cl-pointers ext:*before-save-initializations*)
;;   #-(or clisp sbcl cmu)
;;   (warn "Don't know how to setup a hook before saving cores on this Lisp."))
;; 
;; (defparameter *cl-extension-resetter-list* nil)
;; ;;; FIXME? There's a possible race condition here, but this function
;; ;;; is intended to be called while saving an image, so if someone is
;; ;;; still calling CL functions we lose anyway...
;; (defun reset-cl-pointers ()
;;   (format t "~&;; resetting OpenCL extension pointers...~%")
;;   (mapc #'funcall *cl-extension-resetter-list*)
;;   (setf *cl-extension-resetter-list* nil))
