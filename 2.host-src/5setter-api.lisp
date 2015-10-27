
(in-package :eazy-opencl.host)

(defun set-kernel-arg (kernel index value type)
  (with-foreign-object (p type)
    (setf (mem-ref p type) value)
    (%ocl/e:set-kernel-arg kernel index (foreign-type-size type) p)))

;; (define-info-getter get-kernel-exec-info (kernel param) (%ocl:kernel-exec-info)
;;   (:kernel-exec-info-svm-ptrs (:pointer :void) :array t)
;;   (:kernel-exec-info-svm-fine-grain-system %ocl:bool))
