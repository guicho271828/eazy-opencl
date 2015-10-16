;;; Thin API wrapper over %cl/e apis, mainly on clCreateXXX

(in-package #:eazy-opencl.host)

;;; OpenCL Platform Layer
;;;; contexts

;; todo: error callback, better handling of properties stuff
;; properties arg is ugly since it mixes pointers with enums
;; only one option though, so handling it explicitly for now
(defun create-context #.`(devices &rest properties
                                  &key ,@(enum-keywords-as-symbols '%cl:context-properties))
       ;; #.(documentation 'create-context 'function)
  (declare #.`(ignore ,@(enum-keywords-as-symbols '%cl:context-properties)))
  (with-opencl-plist (props '%cl:context-properties properties)
    (with-foreign-array (devs '%cl:device-id devices)
      (%cl/e:create-context props (length devices) devs
                            (null-pointer)
                            (null-pointer)))))

(defun create-context-from-type #.`(type &rest properties
                                         &key ,@(enum-keywords-as-symbols '%cl:context-properties))
       ;; #.(documentation 'create-context-from-type 'function)
  (declare #.`(ignore ,@(enum-keywords-as-symbols '%cl:context-properties)))
  (with-opencl-plist (props '%cl:context-properties properties)
    (%cl/e:create-context-from-type props type
                                    (null-pointer)
                                    (null-pointer))))

;;; OpenCL Runtime
;;;; Command Queues

(defun create-command-queue (context device &optional properties)
  ;; #.(documentation 'create-context-from-type 'function)
  "This interface is deprecated in OpenCL2.0 !
Properties should be a list of :out-of-order-exec-mode-enable and :profiling-enable."
  ;; something like above should be autogenerated when defining create-context-from-type
  (simple-style-warning "This interface is deprecated in opencl 2.0! Use create-command-queue-with-properties.")
  (%cl/e:create-command-queue context device properties))

#+opencl-2.0
(defun create-command-queue-with-properties #.`(context device &rest properties
                                                        &key ,@(enum-keywords-as-symbols '%cl:queue-properties))
    ;; #.(documentation 'create-command-queue-with-properties 'function)
  (declare #.`(ignore ,@(enum-keywords-as-symbols '%cl:queue-properties)))
  (with-opencl-plist (props '%cl:queue-properties properties)
    (%cl/e:create-command-queue-with-properties context device props)))

;;;; Buffer

(defun create-buffer (context flags size &optional (host-ptr (null-pointer)))
  ;; #.(documentation 'create-buffer 'function)
  (%cl/e:create-buffer context flags size host-ptr))

;;;; Image (TODO: test it)

;; further abstraction should be done in level3
(defun create-image (context flags image-format image-desc &optional (host-ptr (null-pointer)))
  (%cl/e:create-image context flags image-format image-desc host-ptr))

;;;; Pipes

(defun create-pipe #.`(context flags packet-size max-packets &rest properties
                               &key ,@(enum-keywords-as-symbols '%cl:pipe-properties))
  ;; in opencl2.0, properties should be nil
  #+opencl-2.0
  (%cl/e:create-pipe context flags packet-size max-packets (null-pointer))
  #+opencl-2.1
  (with-opencl-plist (props '%cl:pipe-properties properties)
    (%cl/e:create-pipe context flags packet-size max-packets props)))

;;;; SVM (TODO)


;;;; Sampler (TODO: test it)
(defun create-sampler (context normalized-coords addressing-mode filter-mode)
  ;; #.(documentation 'create-context-from-type 'function)
  "This interface is deprecated in OpenCL2.0 !
Properties should be a list of :out-of-order-exec-mode-enable and :profiling-enable."
  ;; something like above should be autogenerated when defining create-context-from-type
  #+opencl-2.0
  (simple-style-warning "This interface is deprecated in opencl 2.0! Use create-sampler-with-properties.")
  (%cl/e:create-sampler context normalized-coords addressing-mode filter-mode))

#+opencl-2.0
(defun create-sampler-with-properties #.`(context &rest properties
                                          &key ,@(enum-keywords-as-symbols '%cl:sampler-properties))
       ;; #.(documentation 'create-sampler 'function)
  (declare #.`(ignore ,@(enum-keywords-as-symbols '%cl:sampler-properties)))
  (with-opencl-plist (props '%cl:sampler-properties properties)
    (%cl/e:create-sampler-with-properties context props)))

;;;; Program

;; I don't see any benefit in passing multiple strings to
;; create-program-with-source.

#+nil
(defun slurp (stream)
  "http://www.ymeme.com/slurping-a-file-common-lisp-83.html"
  (let ((seq (make-array (file-length stream)
                         :element-type 'character
                         :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(declaim (ftype (function (t (or string stream)) *) load-source))
(defun create-program-with-source (context source)
  "Wrapper around create-program-with-source. Source accepts a string or a stream."
  (flet ((load-into-string (length)
           (let ((seq (make-array length :element-type 'character :fill-pointer t)))
             (setf (fill-pointer seq) (read-sequence seq source))
             (values seq (/= (fill-pointer seq) length)))))
    (ematch source
      ((type string)
       (with-foreign-string (cstring source)
         (with-foreign-object (p :pointer 1)
           (setf (mem-ref p :pointer) cstring)
           (%cl/e:create-program-with-source context 1 p (null-pointer)))))
      ((type file-stream)
       (assert (input-stream-p source))
       (load-source context (load-into-string (file-length source))))
      ((type stream)
       (assert (input-stream-p source))
       (let* ((len 0)
              (cstrings (iter (for (values str eof-p) = (load-into-string 4096))
                              (collect (foreign-string-alloc str) result-type simple-vector)
                              (incf len)
                              (until eof-p))))
         (declare (fixnum len))
         (unwind-protect
             (with-foreign-object (p :pointer len)
               (loop for i below len
                     do (setf (mem-aref p :pointer i) (elt cstrings i)))
               (%cl/e:create-program-with-source context len p (null-pointer)))
           (map nil #'foreign-string-free cstrings)))))))

;; todo: create-program-with-binary
;; todo: create-program-with-builtin-kernels
;; todo: create-program-with-IL

;; todo: add notify callback support
;;  - requiring callers to pass a cffi callback is a bit ugly
;;  - using an interbal cffi callback and trying to map back to caller's
;;    lisp callback is probably nicer API, but harder to implement
;;  - also need to deal with thread safety stuff... not sure if it might
;;    be called from arbitrary threads or not
;; todo: add keywords for know options?
(defun build-program (program &key (devices :all) (options ""))
  (check-type options string)
  (if (eq :all devices)
      (%cl/e:build-program program
                           0 (null-pointer)
                           options
                           (null-pointer) (null-pointer))
      (with-foreign-array (devices-foreign '%cl:device-id devices)
        (%cl/e:build-program program
                             (length devices) devices-foreign
                             options
                             (null-pointer) (null-pointer))))
  program)

#+opencl-1.2
(defun compile-program (program &key devices (options "") header-alist)
  "devices: if nil, compilation is performed on all devices.

header-programs-alist: alist of (header-name . program) ,
 For example, '((\"foo.h\" . #.(SB-SYS:INT-SAP #X7FFFE7B5FFE8)))."
  (check-type options string)
  (with-foreign-array (%header-programs '%cl:program (mapcar #'cdr header-alist))
    (with-foreign-array (%header-names :string (mapcar #'car header-alist))
      (flet ((compile/devices (num devices)
               (%cl/e:compile-program program
                                      num devices
                                      options
                                      (length header-alist) %header-programs %header-names
                                      (null-pointer) (null-pointer))))
        (if devices
            (with-foreign-array (%devices '%cl:device-id devices)
              (compile/devices (length devices) %devices))
            (compile/devices 0 (null-pointer))))))
  program)

#+opencl-1.2
(defun link-program (context &key devices (options "") programs)
  "devices: if not specified, it will be the list of devices associated with context."
  (check-type options string)
  (with-foreign-array (%programs '%cl:program programs)
    (flet ((compile/devices (num devices)
             (%cl/e:link-program context num devices options
                                 (length programs) %programs
                                 (null-pointer) (null-pointer))))
      (if devices
          (with-foreign-array (%devices '%cl:device-id devices)
            (compile/devices (length devices) %devices))
          (compile/devices 0 (null-pointer)))))
  program)

;; TODO: below comment is in the original opencl.lisp by 3b.
;; 
;; ;; nv drivers return :invalid-binary for undefined functions,
;; ;; so treat that like build failure for now...
;; ((:build-program-failure :invalid-binary)
;;  (let ((status (loop for i in (get-program-info program :devices)
;;                      collect (list (get-program-build-info program i :status)
;;                                    (get-program-build-info program i :log)))))
;;    (error "Build-program returned :build-program-failure:~:{~&~s : ~s~}" status))))))

;;;; Kernel


(defun create-kernels-in-program (program)
  ;; fixme: verify calling this twice is the correct way to figure out
  ;; how many kernels are in program...
  (get-counted-list %cl/e:create-kernels-in-program (program) '%cl:kernel))



;;;; Event
