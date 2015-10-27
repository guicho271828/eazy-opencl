;; groveller file

(in-package #:eazy-opencl.grovel)

(include "CL/cl.h")

(cstruct #.(lispify "buffer_region") "cl_buffer_region"
         (:origin "origin" :type size-t)
         (:origin "size" :type size-t))

