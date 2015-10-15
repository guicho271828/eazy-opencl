
(in-package :eazy-opencl.host)

(defun set-kernel-arg (kernel index value type)
  (with-foreign-object (p type)
    (setf (mem-ref p type) value)
    (%cl/e:set-kernel-arg kernel index (foreign-type-size type) p)))

;; (define-info-getter get-kernel-exec-info (kernel param) (%cl:kernel-exec-info)
;;   (:kernel-exec-info-svm-ptrs (:pointer :void) :array t)
;;   (:kernel-exec-info-svm-fine-grain-system %cl:bool))
