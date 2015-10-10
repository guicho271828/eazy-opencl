#|

Opencl interface is completely imperative, so we need a good interface.
Largely forked from cl-opencl-3b.

|#



(in-package :eazy-opencl.host)

;;; non-info-getter

(defun get-supported-image-formats (context flags image-type)
  (get-counted-list %cl:get-supported-image-formats (context flags image-type)
                    '%cl:image-format))

;;; info-getter

(defmacro define-info-getter (name (&rest args) fun &body body)
  (with-gensyms (count-temp foreign-value fsize s param)
    `(defun ,name (,@args)
       (ecase ,param
         ,@(mapcar (curry #'info-getter-case-form args fun) body)))))

(defun info-getter-case-form (args fun form)
  (ematch form
    ((list* pname _ _ _)
     `(,pname ,(%normal-case args fun form)))
    ;;
    ;; using NIL to indicate params that return an array of
    ;; unknown length, detecting it this way is a bit ugly,
    ;; possibly should just move all the ELSE/COLLECT into
    ;; COLLECT (destructuring-bind ... (cond ...)) so we can check
    ;; for presence of 3rd arg directly?
    ((list* pname _ nil _)
     `(,pname ,(%array-unknown-length args fun form)))
    ;;
    ;; special case for remaining exceptions, just embed some
    ;; code directly...
    ((list* pname _ code _)
     `(,pname ,code))
    ;; strings could (should?) probably be implemented in terms of the
    ;; unknown-length array stuff...
    ((list pname :string)
     `(,pname ,(%string-case args fun)))
    ((list pname _)
     `(,pname ,(%simple-case args fun form)))))

(defun %base-type (form)
  (destructuring-bind (&optional pname type count-param flag) form
    (declare (ignorable count-param pname))
    (if (eq flag :plist)
        ;; fixme: any exported way to do this?
        (cffi::canonicalize-foreign-type type)
        type)))

(defun %normal-case (args fun form)
  (let ((base-type (%base-type form)))
    (with-gensyms (foreign-value count-temp)
      (match form
        ((list _ type count-param flag)
         `(let ((,count-temp ,(if (numberp count-param)
                                  count-param
                                  `(,name ,@(butlast args 1) ,count-param))))
            (with-foreign-object (,foreign-value ',base-type ,count-temp)
              (check-return
               (,fun ,@args
                     (* ,(foreign-type-size type) ,count-temp)
                     ,foreign-value
                     (cffi:null-pointer)))
              (loop for i below ,count-temp
                    for v = (mem-aref ,foreign-value ',type i)
                    ,@(when (eq flag :plist)
                        `(for prop = t then (not prop)
                              when (and prop (not (zerop v)))
                              collect (foreign-enum-keyword ',type v)
                              else))
                    collect v))))))))

(defun %array-unknown-length (args fun form)
  (let ((base-type (%base-type form)))
    (with-gensyms (foreign-value count-temp fsize)
      (match form
        ((list _ type nil flag)
         `(with-foreign-object (,fsize '%cl::uint)
            (check-return (,fun ,@args
                                0 (cffi::null-pointer)
                                ,fsize))
            (let ((,count-temp (floor (mem-aref ,fsize '%cl::uint)
                                      ,(foreign-type-size type))))
              (with-foreign-object (,foreign-value ',base-type ,count-temp)
                (check-return
                 (,fun ,@args
                       (* ,(foreign-type-size type)
                          ,count-temp)
                       ,foreign-value
                       (cffi:null-pointer)))
                (loop for i below ,count-temp
                      for v = (mem-aref ,foreign-value ',base-type i)
                      ,@ (when (eq flag :plist)
                           `(for prop = t then (not prop)
                                 when (and prop (not (zerop v)))
                                 collect (foreign-enum-keyword ',type v)
                                 else))
                      collect v)))))))))

(defun %string-case (args fun)
  (with-gensyms (count-temp fsize s)
    `(with-foreign-object (,fsize '%cl::uint)
       (check-return (,fun ,@args
                           0 (cffi::null-pointer)
                           ,fsize))
       (let ((,count-temp (mem-aref  ,fsize'%cl::uint)))
         (with-foreign-object (,s :uchar (1+ ,count-temp))
           (check-return (,fun ,@args (1+ ,count-temp)
                               ,s
                               (cffi::null-pointer)))
           (foreign-string-to-lisp ,s))))))

(defun %simple-case (args fun form)
  (with-gensyms (foreign-value)
    (match form
      ((list _ type)
       `(with-foreign-object (,foreign-value ',type)
          (check-return
           (,fun ,@args
                 ,(foreign-type-size type)
                 ,foreign-value
                 (cffi:null-pointer)))
          (mem-aref ,foreign-value ',type))))))

;;; info-getter definitions

(define-info-getter get-device-info (device-id param) %cl:get-device-info
  (:type %cl:device-type)
  (:vendor-id %cl::uint)
  (:max-compute-units %cl::uint)
  (:max-work-item-dimensions %cl::uint)
  (:max-work-item-sizes %cl:size-t :max-work-item-dimensions)
  (:max-work-group-size %cl:size-t)
  (:preferred-vector-width-char %cl::uint)
  (:preferred-vector-width-short %cl::uint)
  (:preferred-vector-width-int %cl::uint)
  (:preferred-vector-width-long %cl::uint)
  (:preferred-vector-width-float %cl::uint)
  (:preferred-vector-width-double %cl::uint)
  (:max-clock-frequency %cl::uint)
  (:address-bits %cl::uint)
  (:max-read-image-args %cl::uint)
  (:max-write-image-args %cl::uint)
  (:max-mem-alloc-size %cl::ulong)
  (:image2d-max-width %cl:size-t)
  (:image2d-max-height %cl:size-t)
  (:image3d-max-width %cl:size-t)
  (:image3d-max-height %cl:size-t)
  (:image3d-max-depth %cl:size-t)
  (:image-support %cl:bool)
  (:max-parameter-size %cl:size-t)
  (:max-samplers %cl::uint)
  (:mem-base-addr-align %cl::uint)
  (:min-data-type-align-size %cl::uint)
  (:single-fp-config %cl:device-fp-config)
  (:global-mem-cache-type %cl:device-mem-cache-type)
  (:global-mem-cacheline-size %cl::uint)
  (:global-mem-cache-size %cl::ulong)
  (:global-mem-size %cl::ulong)
  (:max-constant-buffer-size %cl::ulong)
  (:max-constant-args %cl::uint)
  (:local-mem-type %cl:device-local-mem-type)
  (:local-mem-size %cl::ulong)
  (:error-correction-support %cl:bool)
  (:profiling-timer-resolution %cl:size-t)
  (:endian-little %cl:bool)
  (:available %cl:bool)
  (:compiler-available %cl:bool)
  (:execution-capabilities %cl:device-exec-capabilities)
  (:queue-properties %cl:command-queue-properties)
  (:name :string)
  (:vendor :string)
  (:driver-version :string)
  (:profile :string)
  (:version :string)
  (:extensions :string)
  (:platform %cl:platform-id)
  ;; opencl 1.1
  (:host-unified-memory %cl:bool)
  (:preferred-vector-width-half %cl::uint)
  (:native-vector-width-half %cl::uint)
  (:native-vector-width-char %cl::uint)
  (:native-vector-width-short %cl::uint)
  (:native-vector-width-int %cl::uint)
  (:native-vector-width-long %cl::uint)
  (:native-vector-width-float %cl::uint)
  (:native-vector-width-double %cl::uint)
  (:opencl-c-version :string))

(define-info-getter get-context-info (context param) %cl:get-context-info
  (:reference-count %cl::uint)
  (:devices %cl:device-id ())
  ;; fixme: support plist stuff: alternating enum/value pairs terminated by a single 0
  (:properties %cl:context-properties () :plist))

(define-info-getter get-command-queue-info (command-queue param) %cl:get-command-queue-info
  (:context %cl:context)
  (:device %cl:device-id)
  (:reference-count %cl::uint)
  (:properties %cl:command-queue-properties))

(define-info-getter get-mem-object-info (memobj param) %cl:get-mem-object-info
  (:type %cl:mem-object-type)
  (:flags %cl:mem-flags)
  (:size %cl:size-t)
  (:host-ptr (:pointer :void))
  (:map-count %cl::uint)
  (:reference-count %cl::uint)
  (:context %cl:context)
  ;; 1.1
  (:associated-memobject %cl:mem)
  (:offset %cl:size-t))


(define-info-getter get-image-info (image param) %cl:get-image-info
  (:format %cl:image-format)
  (:element-size %cl:size-t)
  (:row-pitch %cl:size-t)
  (:slice-pitch %cl:size-t)
  (:width %cl:size-t)
  (:height %cl:size-t)
  (:depth %cl:size-t))


(define-info-getter get-sampler-info (sampler param) %cl:get-sampler-info
  (:reference-count %cl::uint)
  (:context %cl:context)
  (:normalized-coords %cl:bool)
  (:addressing-mode %cl:addressing-mode)
  (:filter-mode %cl:filter-mode))


(define-info-getter get-program-info (program param) %cl:get-program-info
  (:reference-count %cl::uint)
  (:context %cl:context)
  (:num-devices %cl::uint)
  (:devices %cl:device-id ())
  (:source :string)
  (:binary-sizes %cl:size-t ())
  (:binaries
   nil
   ;; fixme: test this...
   (let* ((sizes (get-program-info program :binary-sizes))
          (total-size (reduce '+ sizes)))
     (with-foreign-pointer (buffer total-size)
       (with-foreign-object (pointers '(:pointer :void) (length sizes))
         (loop for j = 0 then (+ size j)
            for size in sizes
            for i from 0
            do (setf (mem-aref pointers :pointer i) (inc-pointer buffer j)))
         (check-return
          (%cl:get-program-info program :binaries
                                (* (foreign-type-size :pointer) (length sizes))
                                pointers
                                (cffi:null-pointer)))
         (loop for i from 0
            for size in sizes
            collect
              (loop with array = (make-array size
                                             :element-type '(unsigned-byte 8))
                 for j below size
                 do (setf (aref array j)
                          (mem-aref (mem-aref pointers :pointer i)
                                    :uchar j))
                 finally (return array))))))))


(define-info-getter get-program-build-info (program device param)
    %cl:get-program-build-info
  (:status %cl:build-status)
  (:options :string)
  (:log :string))

(define-info-getter get-kernel-info (kernel param) %cl:get-kernel-info
  (:function-name :string)
  (:num-args %cl::uint)
  (:reference-count %cl::uint)
  (:context %cl:context)
  (:program %cl:program))

(define-info-getter get-kernel-work-group-info (kernel device param)
    %cl:get-kernel-work-group-info
  (:work-group-size %cl:size-t)
  (:compile-work-group-size %cl:size-t 3)
  (:local-mem-size %cl::ulong))

(define-info-getter get-event-info (event param) %cl:get-event-info
  (:command-queue %cl:command-queue)
  (:command-type %cl:command-type)
  (:command-execution-status %cl:int)
  (:reference-count %cl::uint))

(define-info-getter get-event-profiling-info (event param)
    %cl:get-event-profiling-info
  (:queued %cl::ulong)
  (:submit %cl::ulong)
  (:start %cl::ulong)
  (:end %cl::ulong))




