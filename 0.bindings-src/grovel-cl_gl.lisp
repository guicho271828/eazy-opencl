;; cl_khr_gl_sharing

(defcenum (gl-context-info uint)
  (:current-device-for-gl-context-khr #x2006)
  (:devices-for-gl-context-khr        #x2007))



;; cl_gl.h

;; fixme: import these from cl-opengl?
;; (would probably want to split out the GL stuff to a separate .asd if it
;;  depended on cl-opengl though, so just leaving here for now)
(defctype gl-enum :unsigned-int)
(defctype gl-uint :unsigned-int)
(defctype gl-int :int)

;; not sure if it would be better to just use gl:enum here or keep it specific
(defcenum (gl-texture-target uint)
  (:texture-2d #x0de1)
  (:texture-cube-map-positive-x #x8515)
  (:texture-cube-map-positive-y #x8517)
  (:texture-cube-map-positive-z #x8519)
  (:texture-cube-map-negative-x #x8516)
  (:texture-cube-map-negative-y #x8518)
  (:texture-cube-map-negative-z #x851a)
  (:texture-rectangle #x84f5)
  (:texture-rectangle-arb #x84f5)
  (:texture-3d #x806F))

(defcenum (gl-object-type uint)
  (:buffer       #x2000)
  (:texture-2d   #x2001)
  (:texture-3d   #x2002)
  (:renderbuffer #x2003))

(defcenum (gl-texture-info uint)
  (:texture-target #x2004)
  (:mipmap-level   #x2005))

;; clCreateContext, clCreateContextFromType, and clGetGLContextInfoKHR
;; CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR
;; -1000
;; 
;; clGetGLContextInfoKHR
;; CL_CURRENT_DEVICE_FOR_GL_CONTEXT_KHR
;;  0x2006
;; CL_DEVICES_FOR_GL_CONTEXT_KHR
;;  0x2007

;; clCreateContext and clCreateContextFromTypeCL_GL_CONTEXT_KHR
;; CL_EGL_DISPLAY_KHR
;; CL_GLX_DISPLAY_KHR
;; CL_WGL_HDC_KHR
;; CL_CGL_SHAREGROUP_KHR

;; clCreateEventFromGLsyncKHR
