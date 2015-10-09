(in-package #:eazy-opencl.bindings)

(cl:when (cl:boundp 'opencl-1.0) (cl:push :opencl-1.0 cl:*features*))
(cl:when (cl:boundp 'opencl-1.1) (cl:push :opencl-1.1 cl:*features*))
(cl:when (cl:boundp 'opencl-1.2) (cl:push :opencl-1.2 cl:*features*))
(cl:when (cl:boundp 'opencl-2.0) (cl:push :opencl-2.0 cl:*features*))
(cl:when (cl:boundp 'opencl-2.1) (cl:push :opencl-2.1 cl:*features*))
