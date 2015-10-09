;; groveller file

(in-package #:eazy-opencl.bindings)

(include "CL/cl.h")

(cstruct (#.(lispify "cl_buffer_region") "cl_buffer_region")
         (:origin "origin" :type "size_t")
         (:origin "size" :type "size_t"))

;;(cffi:defcstruct _buffer-region
;;  (origin size-t)
;;  (size size-t))
;;(cffi::defctype buffer-region _buffer-region)

;; (defcenum (buffer-create-type uint)
;;   (:region #x1220))

