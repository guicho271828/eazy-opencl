#|
This file is a part of eazy-opencl project.
Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)

Design:

* Garbage collection policy (resource.lisp)

We do not use RETAIN interface, and instead use the garbage collector of
the host lisp. If the object is unreachable from the memory space of the
host lisp, then it RELEASE the object once, decrementing the reference
count. (There might be more references from the opencl
implementation itself, in which case the object is not freed immediately.)
This is realized through trivial-garbage:finalize.

* Convert the imperative C apis -> functional apis. (functional.lisp)

Error handling is already achieved by the previous layer.
What we do now is to convert to/from foreign type,
or to handle the apis which sets the result to the given pointer.

* Class Wrapper?

Context, program, kernel, device, commandqueue can be wrapped in a CLOS
layer. The overhead must be smaller than the large computation done by
opencl.

This does not mean that the wrapper should be very fast.  If this overhead
is a problem, then it also means that your application contains a large
number of very costly GPU-MEMORY communications, required each time the
GPGPU computation is initialized. In such cases, you should instead
consider changing the algorithm or even stop using GPGPU.

|#

(in-package :cl-user)
(defpackage eazy-opencl.host
  (:use :cl :cffi :iterate :alexandria :trivia :trivial-garbage)
  (:export
   #:get-platform-info
   #:get-device-info
   #:get-context-info
   #:get-command-queue-info
   #:get-mem-object-info
   #:get-image-info
   #:get-sampler-info
   #:get-program-info
   #:get-program-build-info
   #:get-kernel-info
   #:get-kernel-work-group-info
   #:get-event-info
   #:get-event-profiling-info
   #:context
   #:context-from-type
   #:command-queue
   #:command-queue-with-properties
   #:buffer
   #:load-source
   #:build-program
   #:create-kernels-in-program
   #:get-platform-ids
   #:get-device-ids
   #:get-supported-image-formats))
