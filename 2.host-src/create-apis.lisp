;;; mainly on clCreateXXX functions

(in-package #:eazy-opencl.host)

;;; 4.3 contexts

;; todo: error callback, better handling of properties stuff
;; properties arg is ugly since it mixes pointers with enums
;; only one option though, so handling it explicitly for now
(defun context (devices
                callback
                &rest properties
                &key
                  platform
                  interop-user-sync
                  ;; gl/dx-related args are not added here
                  &allow-other-keys)
  "Unlike original create-context, DEVICES can be a single atom, in which
case converted into (list devices)"
  (declare (ignore callback platform interop-user-sync))
  ;; (declare (ignore gl-context-khr glx-display-khr
  ;;                  wgl-hdc-khr cgl-sharegroup-khr egl-display-khr
  ;;                  context-property-use-cgl-sharegroup-apple))
  (let ((devices (alexandria:ensure-list devices)))
    (with-opencl-plist (props '%cl:context-properties properties)
      (with-foreign-array (devs '%cl:device-id devices)
        (create-context props (length devices) devs
                        (null-pointer)
                        (null-pointer))))))

(defun context-from-type (type &rest properties &key platform)
  (declare (ignore callback platform interop-user-sync))
  (with-opencl-plist (props '%cl:context-properties properties)
    (create-context-from-type props type
                              (null-pointer)
                              (null-pointer))))

;;; 5.1 command queues

;; (defun convert-cqp-bitfield (properties)
;;    ;; (:profiling-enable t :out-of-order-exec-mode-enable nil) => (:profiling-enable)
;;    ;; for bitfield conversion
;;   (mapcar #'car (remove-if-not #'car (plist-alist properties))))

(defun command-queue (context device &optional properties)
  "This interface is deprecated in opencl 2.0!
Properties should be a list of :out-of-order-exec-mode-enable and :profiling-enable."
  #+opencl-2.0
  (format *error-output* "This interface is deprecated in opencl 2.0! Use create-command-queue-with-properties.")
  (assert (every (lambda (el)
                   (member el '(:out-of-order-exec-mode-enable
                                :profiling-enable)))
                 properties))
  (create-command-queue context device properties))

#+opencl-2.0
(defun command-queue-with-properties (context device &rest properties &key queue-properties queue-size)
  (declare (ignore queue-properties queue-size))
  (with-opencl-plist (props '%cl:queue-properties properties)
    (create-command-queue-with-properties context device props)))



;;; 5.2.1 Creating Buffer Objects

;; fixme: should this support preallocated host memory area?
;; skipping for now, since it exposes ffi level stuff...
;; possibly just support copy-host-ptr mode, with copy from lisp array?
(defun buffer (context flags size &optional (host-ptr (null-pointer)))
  (create-buffer context flags size host-ptr))

;;; 5.3.1 Creating Image Objects


;;; 5.6.1 Creating Program Objects

;; I don't see any benefit in passing multiple strings to
;; create-program-with-source.

(defun load-source (context source)
  (check-type source string)
  (with-foreign-string (cstring source)
    (with-foreign-object (p :pointer)
      (setf (mem-ref p :pointer) cstring)
      (create-program-with-source context 1 p (null-pointer)))))

;; todo: create-program-with-binary
;; todo: create-program-with-builtin-kernels
;; todo: create-program-with-IL

;;; 5.6.2 Building Program Executables

;; todo: add notify callback support
;;  - requiring callers to pass a cffi callback is a bit ugly
;;  - using an interbal cffi callback and trying to map back to caller's
;;    lisp callback is probably nicer API, but harder to implement
;;  - also need to deal with thread safety stuff... not sure if it might
;;    be called from arbitrary threads or not
;; todo: add keywords for know options?
(defun build-program (program &key devices (options ""))
  (check-type options string)
  (with-foreign-array (devices-foreign '%cl:device-id devices)
    (%cl/e:build-program program (length devices)
                         devices-foreign
                         options
                         (null-pointer) (null-pointer))))

;; TODO: below comment is in the original opencl.lisp by 3b.
;; 
;; ;; nv drivers return :invalid-binary for undefined functions,
;; ;; so treat that like build failure for now...
;; ((:build-program-failure :invalid-binary)
;;  (let ((status (loop for i in (get-program-info program :devices)
;;                      collect (list (get-program-build-info program i :status)
;;                                    (get-program-build-info program i :log)))))
;;    (error "Build-program returned :build-program-failure:~:{~&~s : ~s~}" status))))))

;;; 5.7.1 Creating Kernel Objects


(defun create-kernels-in-program (program)
  ;; fixme: verify calling this twice is the correct way to figure out
  ;; how many kernels are in program...
  (get-counted-list %cl:create-kernels-in-program (program) '%cl:kernel))
