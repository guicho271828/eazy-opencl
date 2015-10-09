(cl:in-package #:eazy-opencl.bindings)

(defclfun ("clGetImageInfo" get-image-info) error-code
  (image mem)
  (param-name image-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))


#+(or opencl-1.0 opencl-1.1)
(defclfun ("clCreateImage2D" create-image-2d) mem
  (context context)
  (flags mem-flags)
  (image-format (:pointer image-format))
  (image-width size-t)
  (image-height size-t)
  (image-row-pitch size-t)
  (host-ptr (:pointer :void))
  (errcode-ret (:pointer error-code)))

#+(or opencl-1.0 opencl-1.1)
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
  (image-formats (:pointer image-format))
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
