
(in-package :eazy-opencl.host)

(defun set-kernel-arg (kernel index value type)
  (with-foreign-object (p type)
    (setf (mem-ref p type) value)
    (%cl/e:set-kernel-arg kernel index (foreign-type-size type) p)))
