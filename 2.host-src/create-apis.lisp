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
                        (cffi:null-pointer)
                        (cffi:null-pointer))))))

(defun context-from-type (type &rest properties &key platform)
  (declare (ignore callback platform interop-user-sync))
  (with-opencl-plist (props '%cl:context-properties properties)
    (create-context-from-type props type
                              (cffi:null-pointer)
                              (cffi:null-pointer))))

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


