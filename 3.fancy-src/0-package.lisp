

(defpackage :eazy-opencl.fancy
  (:use :cl :alexandria :iterate :trivia :eazy-opencl.host)
  (:import-from :eazy-opencl.bindings
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
                :boxed-sub-buffer)
  (:export
   #:call-with-easy-opencl-setup
   #:with-easy-opencl-setup))

