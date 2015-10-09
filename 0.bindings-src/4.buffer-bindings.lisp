(cl:in-package #:eazy-opencl.bindings)
;;; standard
(defclfun ("clCreateBuffer" create-buffer) mem
  (context context)
  (flags mem-flags)
  (size size-t)
  (host-ptr (:pointer :void))
  (errcode-ret (:pointer error-code)))

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
  (cb size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueWriteBuffer" enqueue-write-buffer) error-code
  (command-queue command-queue)
  (buffer mem)
  (blocking-write-p bool)
  (offset size-t)
  (cb size-t)
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
  (cb size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(defclfun ("clEnqueueMapBuffer" enqueue-map-buffer) (:pointer :void)
  (command-queue command-queue)
  (buffer mem)
  (blocking-map-p bool)
  (map-flags map-flags)
  (offset size-t)
  (cb size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event))
  (errcode-ret (:pointer error-code)))

;;; Rect
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
