;;;; based on files with following copyright:
;;;;
;;;; * Copyright (c) 2008-2009 The Khronos Group Inc.
;;;; *
;;;; * Permission is hereby granted, free of charge, to any person obtaining a
;;;; * copy of this software and/or associated documentation files (the
;;;; * "Materials"), to deal in the Materials without restriction, including
;;;; * without limitation the rights to use, copy, modify, merge, publish,
;;;; * distribute, sublicense, and/or sell copies of the Materials, and to
;;;; * permit persons to whom the Materials are furnished to do so, subject to
;;;; * the following conditions:
;;;; *
;;;; * The above copyright notice and this permission notice shall be included
;;;; * in all copies or substantial portions of the Materials.
;;;; *
;;;; * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.

(cl:in-package #:eazy-opencl.bindings)

(defclfun ("clCreateKernelsInProgram" create-kernels-in-program) error-code
  (program program)
  (num_kernels uint)
  (kernels (:pointer kernel))
  (num-kernels-ret (:pointer uint)))

(defclfun ("clCreateContext" create-context) context
  (properties (:pointer context-properties))
  (num-devices uint)
  (devices (:pointer device-id))
  (pfn-notify :pointer) ;; fixme: full type?
  (user-data (:pointer :void))
  (errcode-ret (:pointer error-code)))

(defclfun ("clGetProgramBuildInfo" get-program-build-info) error-code
  (program program)
  (device device-id)
  (param-name program-build-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clCreateContextFromType" create-context-from-type) context
  (properties (:pointer context-properties))
  (device-type device-type)
  (pfn-notify :pointer) ;; type?
  (user-data (:pointer :void))
  (errcode-ret (:pointer error-code)))

(defclfun ("clRetainMemObject" retain-mem-object) error-code
  (memobj mem))

;; 1.0 deprecated in 1.1
(defclfun ("clSetCommandQueueProperty" set-command-queue-property)
    error-code
  (command-queue command-queue)
  (properties command-queue-properties)
  (enable bool)
  (old-properties (:pointer command-queue-properties)))

(defclfun ("clSetKernelArg" set-kernel-arg) error-code
  (kernel kernel)
  (arg-index uint)
  (arg-size size-t)
  (arg-value (:pointer :void)))

(defclfun ("clEnqueueMarker" enqueue-marker) error-code
  (command-queue command-queue)
  (event (:pointer event)))

(defclfun ("clEnqueueBarrier" enqueue-barrier) error-code
  (command-queue command-queue))

(defclfun ("clReleaseSampler" release-sampler) error-code
  (sampler sampler))

(defclfun ("clGetDeviceIDs" get-device-ids) error-code
  (platform platform-id)
  (device-type device-type)
  (num-entries uint)
  (devices (:pointer device-id))
  (num-devices (:pointer uint)))

(defclfun ("clFlush" flush) error-code
  (command-queue command-queue))

(defclfun ("clCreateCommandQueue" create-command-queue) command-queue
  (context context)
  (device device-id)
  (properties command-queue-properties)
  (errcode-ret (:pointer error-code)))

(defclfun ("clGetProgramInfo" get-program-info) error-code
  (program program)
  (param-name program-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clReleaseContext" release-context) error-code
  (context context))

(defclfun ("clEnqueueTask" enqueue-task) error-code
  (command-queue command-queue)
  (kernel kernel)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clCreateProgramWithSource" create-program-with-source) program
  (context context)
  (count uint)
  (strings (:pointer :string))
  (lengths (:pointer size-t))
  (errcode-ret (:pointer error-code)))

(defclfun ("clFinish" finish) error-code
  (command-queue command-queue))

(defclfun ("clBuildProgram" build-program) error-code
  (program program)
  (num-devices uint)
  (device-list (:pointer device-id))
  (potions :string)
  (pfn-notify :pointer) ;; type
  (user-data (:pointer :void)))

(defclfun ("clRetainSampler" retain-sampler) error-code
  (sampler sampler))

(defclfun ("clGetDeviceInfo" get-device-info) error-code
  (device device-id)
  (param-name device-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clRetainContext" retain-context) error-code
  (context context))

(defclfun ("clReleaseCommandQueue" release-command-queue) error-code
  (command-queue command-queue))

(defclfun ("clReleaseEvent" release-event) error-code
  (event event))

(defclfun ("clGetMemObjectInfo" get-mem-object-info) error-code
  (memobj mem)
  (param-name mem-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clGetContextInfo" get-context-info) error-code
  (context context)
  (param-name context-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clEnqueueUnmapMemObject" enqueue-unmap-mem-object) error-code
  (command-queue command-queue)
  (memobj mem)
  (mapped-ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clGetCommandQueueInfo" get-command-queue-info) error-code
  (command-queue command-queue)
  (param-name command-queue-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clRetainKernel" retain-kernel) error-code
  (kernel kernel))

(defclfun ("clGetKernelWorkGroupInfo" get-kernel-work-group-info) error-code
  (kernel kernel)
  (device device-id)
  (param-name kernel-work-group-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clRetainEvent" retain-event) error-code
  (event event))

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

(defclfun ("clGetSamplerInfo" get-sampler-info) error-code
  (sampler sampler)
  (param-name sampler-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))


(defclfun ("clReleaseKernel" release-kernel) error-code
  (kernel kernel))

(defclfun ("clReleaseMemObject" release-mem-object) error-code
  (memobj mem))

(defclfun ("clCreateKernel" create-kernel) kernel
  (program program)
  (kernel-name :string)
  (errcode-ret (:pointer error-code)))

(defclfun ("clGetPlatformIDs" get-platform-ids) error-code
  (num-entries uint)
  (platforms (:pointer platform-id))
  (num-platforms (:pointer uint)))



(defclfun ("clCreateProgramWithBinary" create-program-with-binary) program
  (context context)
  (num-devices uint)
  (device-list (:pointer device-id))
  (lengths (:pointer size-t))
  (binaries (:pointer (:pointer :unsigned-char)))
  (binary-status (:pointer int))
  (errcode-ret (:pointer error-code)))

(defclfun ("clRetainProgram" retain-program) error-code
  (program program))

(defclfun ("clRetainCommandQueue" retain-command-queue) error-code
  (command-queue command-queue))

(defclfun ("clWaitForEvents" wait-for-events) error-code
  (num-events uint)
  (event-list (:pointer event)))

(defclfun ("clGetEventProfilingInfo" get-event-profiling-info) error-code
  (event event)
  (param-name profiling-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clReleaseProgram" release-program) error-code
  (program program))

(defclfun ("clEnqueueWaitForEvents" enqueue-wait-for-events) error-code
  (command-queue command-queue)
  (num-events uint)
  (event-list (:pointer event)))

(defclfun ("clGetPlatformInfo" get-platform-info) error-code
  (platform platform-id)
  (param-name platform-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clGetEventInfo" get-event-info) error-code
  (event event)
  (param-name event-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(defclfun ("clGetKernelInfo" get-kernel-info) error-code
  (kernel-name kernel)
  (param-name kernel-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret :pointer))

(defclfun ("clEnqueueNativeKernel" enqueue-native-kernel) error-code
  (command-queue command-queue)
  (user-func :pointer)
  (args (:pointer :void))
  (cb-args size-t)
  (num-mem-objects uint)
  (mem-list (:pointer mem))
  (args-mem-loc (:pointer (:pointer :void)))
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)
  (event (:pointer event)))

(defclfun ("clCreateSampler" create-sampler) sampler
  (context context)
  (normalized-coords bool)
  (addressing-mode addressing-mode)
  (filter-mode filter-mode)
  (errcode-ret (:pointer error-code)))

(defclfun ("clUnloadCompiler" unload-compiler) error-code)



;;; 1.1
(defclfun ("clCreateUserEvent" create-user-event) event
  (context context)
  (errcode-ret (:pointer error-code)))



(defclfun ("clSetEventCallback" set-event-callback) int
  (event event)
  (command-exec-callback-type int) ;; CL_COMPLETE = 0x0
  (callback :pointer)
  (user-data (:pointer :void)))


(defclfun ("clSetMemObjectDestructorCallback"
               set-mem-object-destructor-callback) int
  (memobj mem)
  (callback :pointer)
  (user-data (:pointer :void)))

(defclfun ("clSetUserEventStatus" set-user-event-status) int
  (event event)
  (execution-status int))

