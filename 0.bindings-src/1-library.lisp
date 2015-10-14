(cl:in-package #:eazy-opencl.bindings)

(define-foreign-library :opencl
  (:darwin (:framework "OpenCL")) ;; ?
  (:windows "OpenCL.dll" :convention :stdcall)
  (:unix (:or "libOpenCL.so" "libOpenCL.so.1" "libOpenCL.so.1.0" )))

(use-foreign-library :opencl)
