
;; it is here, untouched, but it doesnt seem like being used... --- guicho 2015/10/9
;; (defmacro defclextfun ((cname lname) return-type &body args)
;;   (alexandria:with-unique-names (pointer)
;;     `(let ((,pointer (null-pointer)))
;;        (defun ,lname ,(mapcar #'car args)
;;          #++(format t "call ext ~s: ~s~%" ',lname (list ,@(mapcar 'first args)))
;;          (when (null-pointer-p ,pointer)
;;            (setf ,pointer (get-extension-function-address ,cname))
;;            (assert (not (null-pointer-p ,pointer)) ()
;;                    "Couldn't load symbol ~A~%" ,cname)
;;            (format t "Loaded function pointer for ~A: ~A~%" ,cname ,pointer)
;;            (push (lambda () (setf ,pointer (null-pointer)))
;;                  *cl-extension-resetter-list*))
;;          (foreign-funcall-pointer
;;           ,pointer
;;           (:library opencl)
;;           ,@(loop for arg in args
;;                      collect (second arg)
;;                      collect (first arg))
;;           ,return-type)))))
