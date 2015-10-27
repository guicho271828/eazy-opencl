#|
This file is a part of eazy-opencl project.
Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-opencl.host
  (:use :cl :cffi :iterate :alexandria :trivia :trivial-garbage)
  (:nicknames :%ocl/h)
  (:import-from :%ocl/e
                ;; reexport
                #:create-kernel)
  (:export
   ;; getter api
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
   ;; list api
   #:get-platform-ids
   #:get-device-ids
   #:get-supported-image-formats
   ;; setter api
   #:set-kernel-arg
   ;; create api
   #:create-context
   #:create-context-from-type
   #:create-command-queue
   #:create-command-queue-with-properties
   #:create-buffer
   #:create-image
   #:create-pipe
   #:create-sampler
   #:create-sampler-with-properties
   #:create-program-with-source
   #:create-program-with-binary
   #:create-program-with-builtin-kernels
   #:build-program
   #:compile-program
   #:link-program
   #:create-kernels-in-program
   ;; reexport
   #:create-kernel))
