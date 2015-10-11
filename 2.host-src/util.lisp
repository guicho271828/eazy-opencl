
(in-package :eazy-opencl.host)

(defmacro without-fp-traps (&body body)
  ;; FIXME: document why this is needed
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))

;;; with-foreign-array
#|

Several functions use this, e.g. clCreateContext, clCreateCommandQueueWithProperties.
They take "properties" argument, which is a pointer to a 0-terminated array
containing property and property-value alternatingly.

|#

(defmacro with-foreign-array ((pointer-var type sequence) &body body)
  "Convert a sequence to a foreign array, then bound it to pointer-var.

SEQUENCE, TYPE are evaluated.
"
  `(locally
       (declare (inline call-with-foreign-array))
     (call-with-foreign-array
      ,type ,sequence
      (lambda (,pointer-var)
        ,@body))))

(declaim (inline call-with-foreign-array))
(defun call-with-foreign-array (type sequence callback)
  (let ((len (length sequence)))
    (with-foreign-object (pointer type len)
      (loop for i below len
            do
         (setf (mem-aref pointer type i)
               ;; sbcl optimize it away when the type of sequence is known.
               ;; note: this function is inlined.
               (elt sequence i)))
      (funcall callback pointer))))
(declaim (notinline call-with-foreign-array))


;;; with-opencl-plist
#|

Several functions use this, e.g. clCreateContext, clCreateCommandQueueWithProperties.
They take "properties" argument, which is a pointer to a 0-terminated array
containing property and property-value alternatingly.

|#


(defmacro with-opencl-plist ((var type plist) &body body)
  "Several functions use this, e.g. clCreateContext, clCreateCommandQueueWithProperties.
Run BODY where VAR is bound to an OpenCL property list (foreign)
converted from PLIST (evaluated).

TYPE (evaluated).
"
  `(locally
       (declare (inline call-with-opencl-plist))
     (call-with-opencl-plist
      ,plist
      ,type
      (lambda (,var) ,@body))))

(declaim (inline call-with-opencl-plist))
(defun call-with-opencl-plist (properties type callback)
  "Setup the given memory region in foreign-array with a lisp plist, by
destructively modify the region"
  (check-type properties list)
  (let* ((len (length properties))
         ;;(base-len (/ len 2))
         (foreign-len (1+ len)))
    (assert (evenp len))
    (with-foreign-object (foreign-array type foreign-len)
      ;; setup array
      (loop for i below len by 2
            for (p v) on properties by #'cddr
            do (setf (mem-aref foreign-array type i)
                     p
                     (mem-aref foreign-array type (1+ i))
                     (if (pointerp v)
                         (pointer-address v)
                         v)))
      (setf (mem-aref foreign-array type foreign-len) 0)
      (funcall callback foreign-array))))
(declaim (notinline call-with-opencl-plist))



