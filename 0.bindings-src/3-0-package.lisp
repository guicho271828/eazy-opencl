(defpackage :eazy-opencl.bindings
  (:use :cl :cffi :%ocl/g :trivial-garbage :alexandria)
  (:shadow :float :char)
  (:nicknames :%ocl)
  #.(let ((acc '(:export)))
      (do-external-symbols (s :%ocl/g (nreverse acc))
        (push s acc)))
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
   #:opencl-error-code
   #:*defined-opencl-functions*)
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
   :boxed-sub-buffer
   :finalize-box)
  (:export
   ;; function bindings
   :get-platform-ids
   :get-platform-info
   ;; 
   :get-device-ids
   :get-device-info
   :release-device
   ;; 
   :create-context
   :create-context-from-type
   :release-context
   :get-context-info
   ;; 
   :create-command-queue
   :create-command-queue-with-properties
   :release-command-queue
   :get-command-queue-info
   :set-command-queue-property
   :enqueue-marker
   :enqueue-barrier
   :enqueue-wait-for-events
   :enqueue-marker-with-wait-list
   :enqueue-barrier-with-wait-list
   ;; 
   :create-buffer
   :create-sub-buffer
   :enqueue-read-buffer
   :enqueue-write-buffer
   :enqueue-copy-buffer
   :enqueue-map-buffer
   :enqueue-read-buffer-rect
   :enqueue-write-buffer-rect
   :enqueue-copy-buffer-rect
   :enqueue-fill-buffer
   ;; 
   :get-image-info
   :create-image
   :create-image-2d
   :create-image-3d
   :get-supported-image-formats
   :enqueue-write-image
   :enqueue-copy-image-to-buffer
   :enqueue-map-image
   :enqueue-read-image
   :enqueue-copy-buffer-to-image
   :enqueue-copy-image
   :enqueue-fill-image
   ;; 
   :create-pipe
   :get-pipe-info
   ;; 
   :release-mem-object
   :get-mem-object-info
   :enqueue-unmap-mem-object
   :set-mem-object-destructor-callback
   :enqueue-migrate-mem-objects
   ;; 
   :create-sampler
   :create-sampler-with-properties
   :release-sampler
   :get-sampler-info
   ;; 
   :create-program-with-source
   :create-program-with-binary
   :create-program-with-builtin-kernels
   :create-program-with-IL
   :release-program
   :get-program-info
   :build-program
   :compile-program
   :link-program
   :get-program-build-info
   ;; 
   :create-kernel
   :create-kernels-in-program
   :release-kernel
   :set-kernel-arg
   :set-kernel-arg-svm-pointer
   :get-kernel-info
   :set-kernel-exec-info
   :get-kernel-work-group-info
   :get-kernel-arg-info
   :get-kernel-sub-group-info
   :enqueue-task
   :enqueue-nd-range-kernel
   :enqueue-native-kernel
   ;; 
   :release-event
   :wait-for-events
   :get-event-info
   :get-event-profiling-info
   :create-user-event
   :set-user-event-status
   :set-event-callback
   ;; 
   :flush
   :finish
   :unload-compiler
   :unload-platform-compiler
   :get-host-timer
   :get-device-and-host-timer))

