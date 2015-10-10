(cl:in-package #:eazy-opencl.bindings)

#+opencl-2.0
(defclfun ("clCreatePipe" create-pipe) mem
  (context context)
  (flags mem-flags)
  (pipe-packet-size (:pointer uint))
  (pipe-max-packets  (:pointer uint))
  (properties (:pointer pipe-properties))
  (errcode-ret (:pointer error-code)))

#+opencl-2.0
(defclfun ("clGetPipeInfo" get-pipe-info) error-code
  (pipe mem)
  (param-name pipe-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

