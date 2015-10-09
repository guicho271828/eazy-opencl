;; groveller file

(in-package #:eazy-opencl.bindings)

(include "CL/cl.h")

(cstruct (#.(lispify "cl_buffer_region") "cl_buffer_region")
         (:origin "origin" :type "size_t")
         (:origin "size" :type "size_t"))
