;; groveller file
(in-package :eazy-opencl.grovel-utils)

(include "CL/cl.h")

(constant (opencl-1.0 "CL_VERSION_1_0") :optional t)
(constant (opencl-1.1 "CL_VERSION_1_1") :optional t)
(constant (opencl-1.2 "CL_VERSION_1_2") :optional t)
(constant (opencl-2.0 "CL_VERSION_2_0") :optional t)
(constant (opencl-2.1 "CL_VERSION_2_1") :optional t)
