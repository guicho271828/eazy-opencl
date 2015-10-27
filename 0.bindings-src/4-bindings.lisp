;;; Ordered in create-release-retain-get/set-info order

(cl:in-package #:eazy-opencl.bindings)

;;; platform
(defclfun ("clGetPlatformIDs" get-platform-ids) error-code
  (num-entries uint)
  (platforms (:pointer platform-id))
  (num-platforms (:pointer uint)))

(defclfun ("clGetPlatformInfo" get-platform-info) error-code
  (platform platform-id)
  (param-name platform-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;;; device
(defclfun ("clGetDeviceIDs" get-device-ids) error-code
  (platform platform-id)
  (device-type device-type)
  (num-entries uint)
  (devices (:pointer device-id))
  (num-devices (:pointer uint)))

(defclfun ("clGetDeviceInfo" get-device-info) error-code
  (device device-id)
  (param-name device-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;;; context
(defclfun ("clCreateContext" create-context) context
  (properties (:pointer context-properties))
  (num-devices uint)
  (devices (:pointer device-id))
  (pfn-notify :pointer) ;; fixme: full type?
  (user-data (:pointer :void))
  (errcode-ret (:pointer error-code)))

(defclfun ("clCreateContextFromType" create-context-from-type) context
  (properties (:pointer context-properties))
  (device-type device-type)
  (pfn-notify :pointer) ;; type?
  (user-data (:pointer :void))
  (errcode-ret (:pointer error-code)))

(defclfun ("clReleaseContext" release-context) error-code
  (context context))

(defclfun ("clRetainContext" retain-context) error-code
  (context context))

(defclfun ("clGetContextInfo" get-context-info) error-code
  (context context)
  (param-name context-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)));;; program


;;; queue

;; #-opencl-2.0
(defclfun ("clCreateCommandQueue" create-command-queue) command-queue
  (context context)
  (device device-id)
  (properties command-queue-properties)
  (errcode-ret (:pointer error-code)))

#+opencl-2.0
(defclfun ("clCreateCommandQueueWithProperties" create-command-queue-with-properties) command-queue
  (context context)
  (device device-id)
  (properties (:pointer queue-properties))
  (errcode-ret (:pointer error-code)))

(defclfun ("clReleaseCommandQueue" release-command-queue) error-code
  (command-queue command-queue))

(defclfun ("clRetainCommandQueue" retain-command-queue) error-code
  (command-queue command-queue))

(defclfun ("clGetCommandQueueInfo" get-command-queue-info) error-code
  (command-queue command-queue)
  (param-name command-queue-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;; #-opencl-1.1
(defclfun ("clSetCommandQueueProperty" set-command-queue-property)
    error-code
  (command-queue command-queue)
  (properties command-queue-properties)
  (enable bool)
  (old-properties (:pointer command-queue-properties)))

;; #-opencl-1.2
(defclfun ("clEnqueueMarker" enqueue-marker) error-code
  (command-queue command-queue)
  (event (:pointer event)))

;; #-opencl-1.2
(defclfun ("clEnqueueBarrier" enqueue-barrier) error-code
  (command-queue command-queue))

;; #-opencl-2.0
(defclfun ("clEnqueueTask" enqueue-task) error-code
  (command-queue command-queue)
  (kernel kernel)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueUnmapMemObject" enqueue-unmap-mem-object) error-code
  (command-queue command-queue)
  (memobj mem)
  (mapped-ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueNDRangeKernel" enqueue-nd-range-kernel) error-code
  (command-queue command-queue)
  (kernel kernel)
  (work-dim uint)
  (global-work-offset (:pointer size-t))
  (global-work-size (:pointer size-t))
  (local-work-size (:pointer size-t))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

;; #-opencl-1.2
(defclfun ("clEnqueueWaitForEvents" enqueue-wait-for-events) error-code
  (command-queue command-queue)
  (num-events uint)
  (event-list (:pointer event)))

(defclfun ("clEnqueueNativeKernel" enqueue-native-kernel) error-code
  (command-queue command-queue)
  (user-func :pointer)
  (args (:pointer :void))
  (cb-args size-t)
  (num-mem-objects uint)
  (mem-list (:pointer mem))
  (args-mem-loc (:pointer (:pointer :void)))
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)            ;FIXME : spec suggests (:pointer event) ...
  (event (:pointer event)))

#+opencl-1.2
(defclfun ("clEnqueueFillBuffer" enqueue-fill-buffer) error-code
  (command-queue command-queue)
  (buffer mem)
  (pattern (:pointer :void))
  (pattern-size size-t)
  (offset size-t)
  (size size-t)
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)
  (event (:pointer event)))

#+opencl-1.2
(defclfun ("clEnqueueFillImage" enqueue-fill-image) error-code
  (command-queue command-queue)
  (image mem)
  (fill-color (:pointer :void))
  (origin (:pointer size-t))
  (region (:pointer size-t))
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)
  (event (:pointer event)))

#+opencl-1.2
(defclfun ("clEnqueueMigrateMemObjects" enqueue-migrate-mem-objects) error-code
  (command-queue command-queue)
  (num-mem-objects uint)
  (mem-objects (:pointer mem))
  (flags mem-migration-flags)
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)
  (event (:pointer event)))

#+opencl-1.2
(defclfun ("clEnqueueMarkerWithWaitList" enqueue-marker-with-wait-list) error-code
  (command-queue command-queue)
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)
  (event (:pointer event)))

#+opencl-1.2
(defclfun ("clEnqueueBarrierWithWaitList" enqueue-barrier-with-wait-list) error-code
  (command-queue command-queue)
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)
  (event (:pointer event)))

;;; buffer
(defclfun ("clCreateBuffer" create-buffer) mem
  (context context)
  (flags mem-flags)
  (size size-t)
  (host-ptr (:pointer :void))
  (errcode-ret (:pointer error-code)))

#+opencl-1.1
(defclfun ("clCreateSubBuffer" create-sub-buffer) mem
  (buffer mem)
  (flags mem-flags)
  (buffer-create-type buffer-create-type)
  (buffer-create-info (:pointer :void))
  (errcode-ret (:pointer error-code)))

(defclfun ("clEnqueueReadBuffer" enqueue-read-buffer) error-code
  (command-queue command-queue)
  (buffer mem)
  (blocking-read-p bool)
  (offset size-t)
  (size size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueWriteBuffer" enqueue-write-buffer) error-code
  (command-queue command-queue)
  (buffer mem)
  (blocking-write-p bool)
  (offset size-t)
  (size size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueCopyBuffer" enqueue-copy-buffer) error-code
  (command-queue command-queue)
  (src-buffer mem)
  (dst-buffer mem)
  (src-offset size-t)
  (dst-offset size-t)
  (size size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueMapBuffer" enqueue-map-buffer) (:pointer :void)
  (command-queue command-queue)
  (buffer mem)
  (blocking-map-p bool)
  (map-flags map-flags)
  (offset size-t)
  (size size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event))
  (errcode-ret (:pointer error-code)))

#+opencl-1.1
(defclfun ("clEnqueueReadBufferRect" enqueue-read-buffer-rect) int
  (command-queue command-queue)
  (buffer mem)
  (blocking-read-p bool)
  (buffer-origin (:pointer size-t)) ;;[3]
  (host-origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (buffer-row-pitch size-t)
  (buffer-slice-pitch size-t)
  (host-row-pitch size-t)
  (host-slice-pitch size-t)
  (pointer (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

#+opencl-1.1
(defclfun ("clEnqueueWriteBufferRect" enqueue-write-buffer-rect) int
  (command-queue command-queue)
  (buffer mem)
  (blocking-write-p bool)
  (buffer-origin (:pointer size-t)) ;;[3]
  (host-origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (buffer-row-pitch size-t)
  (buffer-slice-pitch size-t)
  (host-row-pitch size-t)
  (host-slice-pitch size-t)
  (pointer :pointer)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

#+opencl-1.1
(defclfun ("clEnqueueCopyBufferRect" enqueue-copy-buffer-rect) int
  (command-queue command-queue)
  (src-buffer mem)
  (dst-buffer mem)
  (src-origin (:pointer size-t)) ;;[3]
  (dst-origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (src-row-pitch size-t)
  (src-slice-pitch size-t)
  (dst-row-pitch size-t)
  (dst-slice-pitch size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

;;; image

(defclfun ("clGetImageInfo" get-image-info) error-code
  (image mem)
  (param-name image-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

#+opencl-1.2
(defclfun ("clCreateImage" create-image) mem
  (context context)
  (flags mem-flags)
  (image-format (:pointer (:struct image-format)))
  (image-desc  (:pointer (:struct image-desc)))
  (host-ptr (:pointer :void))
  (errcode-ret (:pointer error-code)))

;; #-opencl-1.2
(defclfun ("clCreateImage2D" create-image-2d) mem
  (context context)
  (flags mem-flags)
  (image-format (:pointer (:struct image-format)))
  (image-width size-t)
  (image-height size-t)
  (image-row-pitch size-t)
  (host-ptr (:pointer :void))
  (errcode-ret (:pointer error-code)))

;; #-opencl-1.2
(defclfun ("clCreateImage3D" create-image-3d) mem
  (context context)
  (flags mem-flags)
  (image-format :pointer)
  (image-width size-t)
  (image-height size-t)
  (image-depth size-t)
  (image-row-pitch size-t)
  (image-slice-pitch size-t)
  (host-ptr (:pointer :void))
  (error-code-ret (:pointer error-code)))

(defclfun ("clGetSupportedImageFormats" get-supported-image-formats)
    error-code
  (context context)
  (flags mem-flags)
  (image-type mem-object-type)
  (num-entries uint)
  (image-formats (:pointer (:struct image-format)))
  (num-image-format (:pointer uint)))

(defclfun ("clEnqueueWriteImage" enqueue-write-image) error-code
  (command-queue command-queue)
  (image mem)
  (blocking-write bool)
  (origin (:pointer size-t)) ;; todo: special type for these size_t[3] args?
  (region (:pointer size-t))
  (input-row-pitch size-t)
  (input-slice-pitch size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueCopyImageToBuffer" enqueue-copy-image-to-buffer)
    error-code
  (command-queue command-queue)
  (src-image mem)
  (dst-buffer mem)
  (src-origin (:pointer size-t))
  (region (:pointer size-t))
  (dst-offset size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueMapImage" enqueue-map-image) (:pointer :void)
  (command-queue command-queue)
  (image mem)
  (blocking-map bool)
  (map-flags map-flags)
  (origin (:pointer size-t)) ;; [3]
  (region (:pointer size-t)) ;; [3]
  (image-row-pitch (:pointer size-t))
  (image-slice-pitch (:pointer size-t))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event))
  (errcode-ret (:pointer error-code)))

(defclfun ("clEnqueueReadImage" enqueue-read-image) error-code
  (command-queue command-queue)
  (image mem)
  (blocking-read bool)
  (origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (row-pitch size-t)
  (slice-pitch size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))


(defclfun ("clEnqueueCopyBufferToImage" enqueue-copy-buffer-to-image)
    error-code
  (command-queue command-queue)
  (src-buffer mem)
  (dst-image mem)
  (src-offset size-t)
  (dst-origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueCopyImage" enqueue-copy-image) error-code
  (command-queue command-queue)
  (src-image mem)
  (dst-image mem)
  (src-origin (:pointer size-t)) ;;[3]
  (dst-origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

;;; pipe

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

;;; mem


(defclfun ("clRetainMemObject" retain-mem-object) error-code
  (memobj mem))
(defclfun ("clReleaseMemObject" release-mem-object) error-code
  (memobj mem))
(defclfun ("clGetMemObjectInfo" get-mem-object-info) error-code
  (memobj mem)
  (param-name mem-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

#+opencl-1.1
(defclfun ("clSetMemObjectDestructorCallback"
               set-mem-object-destructor-callback) int
  (memobj mem)
  (callback :pointer)
  (user-data (:pointer :void)))

;;; Shared Virtual Memory : TODO

;;; sampler

;; #-opencl-2.0
(defclfun ("clCreateSampler" create-sampler) sampler
  (context context)
  (normalized-coords bool)
  (addressing-mode addressing-mode)
  (filter-mode filter-mode)
  (errcode-ret (:pointer error-code)))

#+opencl-2.0
(defclfun ("clCreateSamplerWithProperties" create-sampler-with-properties) sampler
  (context context)
  (sampler-properties sampler-properties)
  (errcode-ret (:pointer error-code)))

(defclfun ("clReleaseSampler" release-sampler) error-code
  (sampler sampler))

(defclfun ("clRetainSampler" retain-sampler) error-code
  (sampler sampler))

(defclfun ("clGetSamplerInfo" get-sampler-info) error-code
  (sampler sampler)
  (param-name sampler-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;;; program
(defclfun ("clCreateProgramWithSource" create-program-with-source) program
  (context context)
  (count uint)
  (strings (:pointer :string))
  (lengths (:pointer size-t))
  (errcode-ret (:pointer error-code)))

(defclfun ("clCreateProgramWithBinary" create-program-with-binary) program
  (context context)
  (num-devices uint)
  (device-list (:pointer device-id))
  (lengths (:pointer size-t))
  (binaries (:pointer (:pointer :unsigned-char)))
  (binary-status (:pointer int))
  (errcode-ret (:pointer error-code)))

(defclfun ("clCreateProgramWithBuiltInKernels" create-program-with-builtin-kernels) program
  (context context)
  (num-devices uint)
  (device-list (:pointer device-id))
  (kernel-names :string)
  (errcode-ret (:pointer error-code)))

#+opencl-2.1
(defclfun ("clCreateProgramWithIL" create-program-with-IL) program
  (context context)
  (il (:pointer :void))
  (lengths (:pointer size-t))
  (errcode-ret (:pointer error-code)))


(defclfun ("clRetainProgram" retain-program) error-code
  (program program))

(defclfun ("clReleaseProgram" release-program) error-code
  (program program))

(defclfun ("clGetProgramInfo" get-program-info) error-code
  (program program)
  (param-name program-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clBuildProgram" build-program) error-code
  (program program)
  (num-devices uint)
  (device-list (:pointer device-id))
  (options :string)
  (pfn-notify :pointer) ;; FIXME: function pointer type
  (user-data (:pointer :void)))
#+opencl-1.2
(defclfun ("clCompileProgram" compile-program) error-code
  (program program)
  (num-devices uint)
  (device-list (:pointer device-id))
  (options :string)
  (num-input-headers uint)
  (input-headers (:pointer program))
  (header-include-names (:pointer :string))
  (pfn-notify :pointer) ;; FIXME: function pointer type
  (user-data (:pointer :void)))
#+opencl-1.2
(defclfun ("clLinkProgram" link-program) program
  (context context)
  (num-devices uint)
  (device-list (:pointer device-id))
  (options :string)
  (num-input-programs uint)
  (input-programs (:pointer program))
  (pfn-notify :pointer) ;; FIXME: function pointer type
  (user-data (:pointer :void))
  (errcode-ret (:pointer error-code)))

(defclfun ("clGetProgramBuildInfo" get-program-build-info) error-code
  (program program)
  (device device-id)
  (param-name program-build-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))
;;; kernel
(defclfun ("clCreateKernel" create-kernel) kernel
  (program program)
  (kernel-name :string)
  (errcode-ret (:pointer error-code)))

(defclfun ("clCreateKernelsInProgram" create-kernels-in-program) error-code
  (program program)
  (num-kernels uint)
  (kernels (:pointer kernel))
  (num-kernels-ret (:pointer uint)))

(defclfun ("clReleaseKernel" release-kernel) error-code
  (kernel kernel))

(defclfun ("clRetainKernel" retain-kernel) error-code
  (kernel kernel))

(defclfun ("clSetKernelArg" set-kernel-arg) error-code
  (kernel kernel)
  (arg-index uint)
  (arg-size size-t)
  (arg-value (:pointer :void)))

#+opencl-2.0
(defclfun ("clSetKernelArgSVMPointer" set-kernel-arg-svm-pointer) error-code
  (kernel kernel)
  (arg-index uint)
  (arg-value (:pointer :void)))

(defclfun ("clGetKernelInfo" get-kernel-info) error-code
  (kernel-name kernel)
  (param-name kernel-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret :pointer))

#+opencl-2.0
(defclfun ("clSetKernelExecInfo" set-kernel-exec-info) error-code
  (kernel-name kernel)
  (param-name kernel-exec-info)
  (param-value-size size-t)
  (param-value (:pointer :void)))

#+opencl-1.1
(defclfun ("clGetKernelWorkGroupInfo" get-kernel-work-group-info) error-code
  (kernel kernel)
  (device device-id)
  (param-name kernel-work-group-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

#+opencl-1.2
(defclfun ("clGetKernelArgInfo" get-kernel-arg-info) error-code
  (kernel-name kernel)
  (arg-index uint)
  (param-name kernel-arg-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

#+opencl-2.1
(defclfun ("clGetKernelSubGroupInfo" get-kernel-sub-group-info) error-code
  (kernel kernel)
  (device device-id)
  (param-name kernel-sub-group-info)
  (input-value-size size-t)
  (input-value (:pointer :void))
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;;; event
(defclfun ("clReleaseEvent" release-event) error-code
  (event event))

(defclfun ("clRetainEvent" retain-event) error-code
  (event event))

(defclfun ("clWaitForEvents" wait-for-events) error-code
  (num-events uint)
  (event-list (:pointer event)))

(defclfun ("clGetEventInfo" get-event-info) error-code
  (event event)
  (param-name event-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clGetEventProfilingInfo" get-event-profiling-info) error-code
  (event event)
  (param-name profiling-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

#+opencl-1.1
(defclfun ("clCreateUserEvent" create-user-event) event
  (context context)
  (errcode-ret (:pointer error-code)))

#+opencl-1.1
(defclfun ("clSetUserEventStatus" set-user-event-status) int
  (event event)
  (execution-status int))

#+opencl-1.1
(defclfun ("clSetEventCallback" set-event-callback) int
  (event event)
  (command-exec-callback-type int) ;; CL_COMPLETE = 0x0
  (callback :pointer)
  (user-data (:pointer :void)))


;;; others

(defclfun ("clFlush" flush) error-code
  (command-queue command-queue))

(defclfun ("clFinish" finish) error-code
  (command-queue command-queue))

;; #-opencl-1.2
(defclfun ("clUnloadCompiler" unload-compiler) error-code)

#+opencl-1.2
(defclfun ("clUnloadPlatformCompiler" unload-platform-compiler) error-code
  (platform platform-id))


#+opencl-2.1
(defclfun ("clGetHostTimer" get-host-timer) error-code
  (device device-id)
  (host-timestamp (:pointer ulong)))

#+opencl-2.1
(defclfun ("clGetDeviceAndHostTimer" get-device-and-host-timer) error-code
  (device device-id)
  (device-timestamp (:pointer ulong))
  (host-timestamp (:pointer ulong)))


;;; extensions

;;;; GL

;; clCreateEventFromEGLSyncKHR
;; clCreateEventFromGLsyncKHR
;; clCreateFromEGLImageKHR
;; clCreateFromGLBuffer
;; clCreateFromGLRenderbuffer
;; clCreateFromGLTexture
;; clEnqueueAcquireEGLObjectsKHR
;; clEnqueueAcquireGLObjects
;; clEnqueueReleaseEGLObjectsKHR
;; clEnqueueReleaseGLObjects
;; clGetGLObjectInfo
;; clGetGLTextureInfo

;;;; DX11, D3D

;; clCreateFromDX9MediaSurfaceKHR
;; clCreateFromD3D10BufferKHR
;; clCreateFromD3D10Texture2DKHR
;; clCreateFromD3D10Texture3DKHR
;; clCreateFromD3D11BufferKHR
;; clCreateFromD3D11Texture2DKHR
;; clCreateFromD3D11Texture3DKHR

;; clEnqueueAcquireDX9MediaSurfacesKHR
;; clEnqueueAcquireD3D10ObjectsKHR
;; clEnqueueAcquireD3D11ObjectsKHR

;; clEnqueueReleaseDX9MediaSurfacesKHR
;; clEnqueueReleaseD3D10ObjectsKHR
;; clEnqueueReleaseD3D11ObjectsKHR

;; clGetDeviceIDsFromD3D10KHR
;; clGetDeviceIDsFromD3D11KHR
;; clGetDeviceIDsFromDX9MediaAdapterKHR

;;;; others

;; clGetExtensionFunctionAddressForPlatform
;; clGetKernelInfo
;; clGetKernelSubGroupInfoKHR
;; clIcdGetPlatformIDsKHR
;; clTerminateContextKHR
