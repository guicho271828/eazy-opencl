(in-package :eazy-opencl.host)


;;; info-getter definitions

(define-info-getter get-device-info (device-id param) %cl/e:get-device-info
  (:type                          %cl:device-type)
  (:vendor-id                     %cl:uint)
  (:max-compute-units             %cl:uint)
  (:max-work-item-dimensions      %cl:uint)
  (:max-work-item-sizes           %cl:size-t :querysize :max-work-item-dimensions)
  (:max-work-group-size           %cl:size-t)
  (:preferred-vector-width-char   %cl:uint)
  (:preferred-vector-width-short  %cl:uint)
  (:preferred-vector-width-int    %cl:uint)
  (:preferred-vector-width-long   %cl:uint)
  (:preferred-vector-width-float  %cl:uint)
  (:preferred-vector-width-double %cl:uint)
  (:max-clock-frequency           %cl:uint)
  (:address-bits                  %cl:uint)
  (:max-read-image-args           %cl:uint)
  (:max-write-image-args          %cl:uint)
  (:max-mem-alloc-size            %cl:ulong)
  (:image2d-max-width             %cl:size-t)
  (:image2d-max-height            %cl:size-t)
  (:image3d-max-width             %cl:size-t)
  (:image3d-max-height            %cl:size-t)
  (:image3d-max-depth             %cl:size-t)
  (:image-support                 %cl:bool)
  (:max-parameter-size            %cl:size-t)
  (:max-samplers                  %cl:uint)
  (:mem-base-addr-align           %cl:uint)
  (:min-data-type-align-size      %cl:uint)
  (:single-fp-config              %cl:device-fp-config)
  (:global-mem-cache-type         %cl:device-mem-cache-type)
  (:global-mem-cacheline-size     %cl:uint)
  (:global-mem-cache-size         %cl:ulong)
  (:global-mem-size               %cl:ulong)
  (:max-constant-buffer-size      %cl:ulong)
  (:max-constant-args             %cl:uint)
  (:local-mem-type                %cl:device-local-mem-type)
  (:local-mem-size                %cl:ulong)
  (:error-correction-support      %cl:bool)
  (:profiling-timer-resolution    %cl:size-t)
  (:endian-little                 %cl:bool)
  (:available                     %cl:bool)
  (:compiler-available            %cl:bool)
  (:execution-capabilities        %cl:device-exec-capabilities)
  (:queue-properties              %cl:command-queue-properties)
  (:name           :string)
  (:vendor         :string)
  (:driver-version :string)
  (:profile        :string)
  (:version        :string)
  (:extensions     :string)
  (:platform                      %cl:platform-id)
  ;; opencl 1.1
  (:host-unified-memory           %cl:bool)
  (:preferred-vector-width-half   %cl:uint)
  (:native-vector-width-half      %cl:uint)
  (:native-vector-width-char      %cl:uint)
  (:native-vector-width-short     %cl:uint)
  (:native-vector-width-int       %cl:uint)
  (:native-vector-width-long      %cl:uint)
  (:native-vector-width-float     %cl:uint)
  (:native-vector-width-double    %cl:uint)
  (:opencl-c-version :string))

(define-info-getter get-context-info (context param) %cl/e:get-context-info
 (:reference-count %cl:uint)
 (:devices         %cl:device-id :array t)
 (:properties      %cl:context-properties :array t :plist t))
;; fixme : support plist stuff: alternating enum/value pairs terminated by a single 0

(define-info-getter get-command-queue-info (command-queue param) %cl/e:get-command-queue-info
  (:context         %cl:context)
  (:device          %cl:device-id)
  (:reference-count %cl:uint)
  (:properties      %cl:command-queue-properties))

(define-info-getter get-mem-object-info (memobj param) %cl/e:get-mem-object-info
  (:type                 %cl:mem-object-type)
  (:flags                %cl:mem-flags)
  (:size                 %cl:size-t)
  (:host-ptr             (:pointer :void))
  (:map-count            %cl:uint)
  (:reference-count      %cl:uint)
  (:context              %cl:context)
  ;; 1.1
  (:associated-memobject %cl:mem)
  (:offset               %cl:size-t))

(define-info-getter get-image-info (image param) %cl/e:get-image-info
  (:format       %cl:image-format)
  (:element-size %cl:size-t)
  (:row-pitch    %cl:size-t)
  (:slice-pitch  %cl:size-t)
  (:width        %cl:size-t)
  (:height       %cl:size-t)
  (:depth        %cl:size-t))

(define-info-getter get-sampler-info (sampler param) %cl/e:get-sampler-info
  (:reference-count   %cl:uint)
  (:context           %cl:context)
  (:normalized-coords %cl:bool)
  (:addressing-mode   %cl:addressing-mode)
  (:filter-mode       %cl:filter-mode))

(define-info-getter get-program-info (program param) %cl/e:get-program-info
  (:reference-count %cl:uint)
  (:context         %cl:context)
  (:num-devices     %cl:uint)
  (:devices         %cl:device-id :array t)
  (:source          :string)
  (:binary-sizes    %cl:size-t :array t)
  (:binaries
   nil
   :form
   ;; fixme: test this...
   (let* ((sizes (get-program-info program :binary-sizes))
          (total-size (reduce '+ sizes)))
     (with-foreign-pointer (buffer total-size)
       (with-foreign-object (pointers '(:pointer :void) (length sizes))
         (loop for j = 0 then (+ size j)
               for size in sizes
               for i from 0
               do (setf (mem-aref pointers :pointer i) (inc-pointer buffer j)))
         (%cl/e:get-program-info program :binaries
                                 (* (foreign-type-size :pointer) (length sizes))
                                 pointers
                                 (cffi:null-pointer))
         (loop for i from 0
               for size in sizes
               collect
               (loop with array = (make-array size :element-type '(unsigned-byte 8))
                     for j below size
                     do (setf (aref array j)
                              (mem-aref (mem-aref pointers :pointer i) :uchar j))
                     finally (return array))))))))

(define-info-getter get-program-build-info (program device param) %cl/e:get-program-build-info
  (:status  %cl:build-status)
  (:options :string)
  (:log     :string))

(define-info-getter get-kernel-info (kernel param) %cl/e:get-kernel-info
  (:function-name :string)
  (:num-args        %cl:uint)
  (:reference-count %cl:uint)
  (:context         %cl:context)
  (:program         %cl:program))

(define-info-getter get-kernel-work-group-info (kernel device param) %cl/e:get-kernel-work-group-info
  (:work-group-size         %cl:size-t)
  (:compile-work-group-size %cl:size-t :fixedsize 3)
  (:local-mem-size          %cl:ulong))

(define-info-getter get-event-info (event param) %cl/e:get-event-info
  (:command-queue            %cl:command-queue)
  (:command-type             %cl:command-type)
  (:command-execution-status %cl:int)
  (:reference-count          %cl:uint))

(define-info-getter get-event-profiling-info (event param) %cl/e:get-event-profiling-info
  (:queued %cl:ulong)
  (:submit %cl:ulong)
  (:start  %cl:ulong)
  (:end    %cl:ulong))


