
#|


This file, similar to the original opencl.lisp, follows the structure of
opencl 2.0 specification available from the kronos website.


The test scripts in t/ follows the similar structure, and holds several
unit tests.

|#


(in-package :eazy-opencl.host)

(defmacro get-counted-list (fun (&rest args) type)
  (with-gensyms (fcount count buffer i)
    `(with-foreign-object (,fcount '%cl:uint)
       (,fun ,@args 0 (cffi:null-pointer) ,fcount)
       (let ((,count (mem-aref ,fcount '%cl:uint)))
         (when (> ,count 0)
           (with-foreign-object (,buffer ,type (1+ ,count))
             (,fun ,@args (1+ ,count) ,buffer ,fcount)
             (loop for ,i below ,count
                collect (mem-aref ,buffer ,type ,i))))))))

(defmacro without-fp-traps (&body body)
  ;; FIXME: document why this is needed
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))

(defun get-platform-ids ()
  "returns a list of available OpenCL Platform IDs (opaque, don't need to be
manually released)"
  ;; FIXME: figure out if returning same pointer twice is correct,
  ;; possibly remove-duplicates on it if so?
  (without-fp-traps
    (get-counted-list %cl/e:get-platform-ids () '%cl:platform-id)))

(defun get-supported-image-formats (context flags image-type)
  (get-counted-list %cl:get-supported-image-formats (context flags image-type)
                    '%cl:image-format))
