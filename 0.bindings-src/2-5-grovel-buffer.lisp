;; groveller file

(in-package #:eazy-opencl.grovel)

#+darwin
(include "OpenCL/cl.h")
#-darwin
(include "CL/cl.h")

(cstruct #.(lispify "buffer_region") "cl_buffer_region"
         (:origin "origin" :type size-t)
         (:origin "size" :type size-t))

