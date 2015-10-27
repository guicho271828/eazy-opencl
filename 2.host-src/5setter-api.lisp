
(in-package :eazy-opencl.host)

(defun set-kernel-arg (kernel index value type)
  (with-foreign-object (p type)
    (setf (mem-ref p type) value)
    (%ocl/e:set-kernel-arg kernel index (foreign-type-size type) p)))

;; (define-info-getter set-kernel-exec-info (kernel param) (%ocl:kernel-exec-info)
;;   (:kernel-exec-info-svm-ptrs (:pointer :void) :array t)
;;   (:kernel-exec-info-svm-fine-grain-system %ocl:bool))

(defun set-kernel-exec-info (kernel param-name value)
  (ecase param-name
    (:kernel-exec-info-svm-ptrs
     (with-foreign-array (a '(:pointer :void) value size)
       (%ocl/e:set-kernel-exec-info kernel param-name size a)))
    (:kernel-exec-info-svm-fine-grain-system
     (%ocl/e:set-kernel-exec-info kernel param-name (foreign-type-size '%ocl:bool) value))))
