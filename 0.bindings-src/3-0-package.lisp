(defpackage :eazy-opencl.bindings
  (:use :cl :cffi :%ocl/g :trivial-garbage :alexandria)
  (:shadow :float :char)
  (:nicknames :%ocl)
  (:export
   #:buffer
   #:sub-buffer
   #:image
   #:pipe
   :command-queue
   :context
   :device-id
   :event
   :kernel
   :mem
   :program
   :sampler
   :bool
   #:opencl-error
   #:opencl-error-code)
  (:export
   :boxed-command-queue
   :boxed-context
   :boxed-device-id
   :boxed-event
   :boxed-kernel
   :boxed-mem
   :boxed-program
   :boxed-sampler
   :boxed-buffer
   :boxed-image
   :boxed-pipe
   :boxed-sub-buffer))

