
(in-package :eazy-opencl.host)

(defmacro without-fp-traps (&body body)
  ;; FIXME: document why this is needed
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))
