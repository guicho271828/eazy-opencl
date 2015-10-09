;; cl_khr_gl_sharing

(defclextfun ("clGetGLContextInfoKHR" get-gl-context-info-khr) cl-error-codes
  (properties (:pointer context-properties))
  (param-name gl-context-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;; cl_gl.h

(defclfun ("clCreateFromGLBuffer" create-from-gl-buffer) mem
  (context context)
  (flags mem-flags)
  (bufobj gl-uint)
  (errcode-ret (:pointer cl-error-codes)))

(defclfun ("clCreateFromGLTexture2D" create-from-gl-texture-2d) mem
  (context context)
  (flags mem-flags)
  (target gl-texture-target)
  (miplevel gl-int)
  (texture gl-uint)
  (errcode-ret (:pointer cl-error-codes)))

;; 1.0
(defclfun ("clCreateFromGLTexture3D" create-from-gl-texture-3d) mem
  (context context)
  (flags mem-flags)
  (target gl-texture-target)
  (miplevel gl-int)
  (texture gl-uint)
  (errcode-ret (:pointer cl-error-codes)))

;;  1.0
(defclfun ("clCreateFromGLRenderbuffer" create-from-gl-renderbuffer) mem
  (context context)
  (flags mem-flags)
  (renderbuffer gl-uint)
  (errcode-ret (:pointer cl-error-codes)))

;; 1.0
(defclfun ("clGetGLObjectInfo" get-gl-object-info) cl-error-codes
  (memobj mem)
  (gl-object-type (:pointer gl-object-type))
  (gl-object-name (:pointer gl-uint)))

;; 1.0
(defclfun ("clGetGLTextureInfo" get-gl-texture-info) cl-error-codes
  (memobj mem)
  (param-name gl-texture-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;; 1.0
(defclfun ("clEnqueueAcquireGLObjects" enqueue-acquire-gl-objects) cl-error-codes
  (command-queue command-queue)
  (num-objects uint)
  (mem-objects (:pointer mem))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

;; 1.0
(defclfun ("clEnqueueReleaseGLObjects" enqueue-release-gl-objects) cl-error-codes
  (command-queue command-queue)
  (num-objects uint)
  (mem-objects (:pointer mem))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))
