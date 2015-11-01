(in-package :eazy-opencl.host)


;;; info-getter definitions (alphabetical)

;; "returns specified info about a platform-id
;;  param = :profile, :version, :name, :vendor, :extensions"
(define-info-getter get-command-queue-info (command-queue param) (%ocl:command-queue-info)
  (:queue-context         %ocl:context)
  (:queue-device          %ocl:device-id)
  (:queue-reference-count %ocl:uint)
  (:queue-properties      %ocl:command-queue-properties)
  #+opencl-2.0
  (:queue-size            %ocl:uint))

(define-info-getter get-context-info (context param) (%ocl:context-info)
 (:context-reference-count %ocl:uint)
 (:context-devices         %ocl:device-id :array t)
 (:context-properties      %ocl:context-properties :array t :plist t)
 (:context-num-devices     %ocl:uint))
;; fixme : support plist stuff: alternating enum/value pairs terminated by a single 0

(define-info-getter get-device-info (device-id param) (%ocl:device-info)
  (:device-address-bits                         %ocl:uint)
  (:device-available                            %ocl:bool)
  (:device-built-in-kernels                     :string)
  (:device-compiler-available                   %ocl:bool)
  (:device-double-fp-config                     %ocl:device-fp-config)
  (:device-endian-little                        %ocl:bool)
  (:device-error-correction-support             %ocl:bool)
  (:device-execution-capabilities               %ocl:device-exec-capabilities)
  (:device-extensions                           :string)
  (:device-global-mem-cache-size                %ocl:ulong)
  (:device-global-mem-cache-type                %ocl:device-mem-cache-type)
  (:device-global-mem-cacheline-size            %ocl:uint)
  (:device-global-mem-size                      %ocl:ulong)
  #+opencl-2.0
  (:device-global-variable-preferred-total-size %ocl:size-t)
  (:device-host-unified-memory                  %ocl:bool)
  (:device-image-base-address-alignment         %ocl:uint)
  (:device-image-max-array-size                 %ocl:size-t)
  (:device-image-max-buffer-size                %ocl:size-t)
  (:device-image-pitch-alignment                %ocl:uint)
  (:device-image-support                        %ocl:bool)
  (:device-image2d-max-height                   %ocl:size-t)
  (:device-image2d-max-width                    %ocl:size-t)
  (:device-image3d-max-depth                    %ocl:size-t)
  (:device-image3d-max-height                   %ocl:size-t)
  (:device-image3d-max-width                    %ocl:size-t)
  (:device-linker-available                     %ocl:bool)
  (:device-local-mem-size                       %ocl:ulong)
  (:device-local-mem-type                       %ocl:device-local-mem-type)
  (:device-max-clock-frequency                  %ocl:uint)
  (:device-max-compute-units                    %ocl:uint)
  (:device-max-constant-args                    %ocl:uint)
  (:device-max-constant-buffer-size             %ocl:ulong)
  #+opencl-2.0
  (:device-max-global-variable-size             %ocl:size-t)
  (:device-max-mem-alloc-size                   %ocl:ulong)
  (:device-max-on-device-events                 %ocl:uint)
  (:device-max-on-device-queues                 %ocl:uint)
  (:device-max-parameter-size                   %ocl:size-t)
  (:device-max-pipe-args                        %ocl:uint)
  (:device-max-read-image-args                  %ocl:uint)
  (:device-max-read-write-image-args            %ocl:uint)
  (:device-max-samplers                         %ocl:uint)
  (:device-max-work-group-size                  %ocl:size-t)
  (:device-max-work-item-dimensions             %ocl:uint)
  (:device-max-work-item-sizes                  %ocl:size-t :querysize :device-max-work-item-dimensions)
  (:device-max-write-image-args                 %ocl:uint)
  (:device-mem-base-addr-align                  %ocl:uint)
  (:device-min-data-type-align-size             %ocl:uint)
  (:device-name                                 :string)
  (:device-native-vector-width-char             %ocl:uint)
  (:device-native-vector-width-double           %ocl:uint)
  (:device-native-vector-width-float            %ocl:uint)
  (:device-native-vector-width-half             %ocl:uint)
  (:device-native-vector-width-int              %ocl:uint)
  (:device-native-vector-width-long             %ocl:uint)
  (:device-native-vector-width-short            %ocl:uint)
  (:device-opencl-c-version                     :string)
  (:device-parent-device                        %ocl:device-id)
  (:device-partition-affinity-domain            %ocl:device-affinity-domain)
  (:device-partition-max-sub-devices            %ocl:uint)
  (:device-partition-properties                 %ocl:device-partition-property :array t)
  (:device-partition-type                       %ocl:device-partition-property :array t)
  (:device-pipe-max-active-reservations         %ocl:uint)
  (:device-pipe-max-packet-size                 %ocl:uint)
  (:device-platform                             %ocl:platform-id)
  (:device-preferred-global-atomic-alignment    %ocl:uint)
  (:device-preferred-interop-user-sync          %ocl:uint)
  (:device-preferred-local-atomic-alignment     %ocl:uint)
  (:device-preferred-platform-atomic-alignment  %ocl:uint)
  (:device-preferred-vector-width-char          %ocl:uint)
  (:device-preferred-vector-width-double        %ocl:uint)
  (:device-preferred-vector-width-float         %ocl:uint)
  (:device-preferred-vector-width-half          %ocl:uint)
  (:device-preferred-vector-width-int           %ocl:uint)
  (:device-preferred-vector-width-long          %ocl:uint)
  (:device-preferred-vector-width-short         %ocl:uint)
  (:device-printf-buffer-size                   %ocl:size-t)
  (:device-profile                              :string)
  (:device-profiling-timer-resolution           %ocl:size-t)
  #+opencl-2.0
  (:device-queue-on-device-max-size             %ocl:uint)
  #+opencl-2.0
  (:device-queue-on-device-preferred-size       %ocl:uint)
  #+opencl-2.0
  (:device-queue-on-device-properties           %ocl:command-queue-properties)
  #+opencl-2.0
  (:device-queue-on-host-properties             %ocl:command-queue-properties)
  (:device-queue-properties                     %ocl:command-queue-properties)
  (:device-reference-count                      %ocl:uint)
  (:device-single-fp-config                     %ocl:device-fp-config)
  #+opencl-2.0
  (:device-svm-capabilities                     %ocl:device-svm-capabilities)
  (:device-type                                 %ocl:device-type)
  (:device-vendor                               :string)
  (:device-vendor-id                            %ocl:uint)
  (:device-version                              :string)
  (:driver-version                              :string))

(define-info-getter get-event-info (event param) (%ocl:event-info)
  (:event-command-queue            %ocl:command-queue)
  #+opencl-1.1
  (:event-context                  %ocl:context)
  (:event-command-type             %ocl:command-type)
  (:event-command-execution-status %ocl:int)
  (:event-reference-count          %ocl:uint))

(define-info-getter get-event-profiling-info (event param) (%ocl:profiling-info)
  (:profiling-command-queued %ocl:ulong)
  (:profiling-command-submit %ocl:ulong)
  (:profiling-command-start  %ocl:ulong)
  (:profiling-command-end    %ocl:ulong)
  #+opencl-2.0
  (:profiling-command-complete %ocl:ulong))

(define-info-getter get-image-info (image param) (%ocl:image-info)
  (:image-format       (:pointer (:struct %ocl:image-format)))
  (:image-element-size %ocl:size-t)
  (:image-row-pitch    %ocl:size-t)
  (:image-slice-pitch  %ocl:size-t)
  (:image-width        %ocl:size-t)
  (:image-height       %ocl:size-t)
  (:image-depth        %ocl:size-t)
  (:image-num-samples %ocl:uint)
  (:image-num-mip-levels %ocl:uint)
  ;; #-opencl-2.0
  #+opencl-1.2
  (:image-buffer %ocl:buffer) ; deprecated in 2.0
  (:image-array-size %ocl:size-t))

(define-info-getter get-kernel-info (kernel param) (%ocl:kernel-info)
  (:kernel-attributes      :string)
  (:kernel-function-name   :string)
  (:kernel-num-args        %ocl:uint)
  (:kernel-reference-count %ocl:uint)
  (:kernel-context         %ocl:context)
  (:kernel-program         %ocl:program))

(define-info-getter get-kernel-work-group-info (kernel device param) (%ocl:kernel-work-group-info)
  (:kernel-global-work-size                   %ocl:size-t :fixedsize 3)
  (:kernel-work-group-size                    %ocl:size-t)
  (:kernel-compile-work-group-size            %ocl:size-t :fixedsize 3)
  (:kernel-local-mem-size          %ocl:ulong)
  (:kernel-preferred-work-group-size-multiple %ocl:ulong)
  (:kernel-private-mem-size                   %ocl:ulong))


(define-info-getter get-mem-object-info (memobj param) (%ocl:mem-info)
  (:mem-type                 %ocl:mem-object-type)
  (:mem-flags                %ocl:mem-flags)
  (:mem-size                 %ocl:size-t)
  (:mem-host-ptr             (:pointer :void))
  (:mem-map-count            %ocl:uint)
  (:mem-reference-count      %ocl:uint)
  (:mem-context              %ocl:context)
  #+opencl-1.1
  (:mem-associated-memobject %ocl:mem)
  (:mem-offset               %ocl:size-t)
  #+opencl-2.0
  (:mem-uses-svm-pointer  %ocl:bool))

#+opencl-2.0
(define-info-getter get-pipe-info (pipe param) (%ocl:pipe-info)
  (:pipe-packet-size %ocl:uint)
  (:pipe-max-packets %ocl:uint))

(define-info-getter get-platform-info (platform-id param) (%ocl:platform-info)
  (:platform-profile        :string)
  (:platform-version        :string)
  (:platform-name           :string)
  (:platform-vendor         :string)
  (:platform-extensions     :string)
  (:platform-icd-suffix-khr :string))

(define-info-getter get-program-build-info (program device param) (%ocl:program-build-info)
  (:program-build-status                     %ocl:build-status)
  (:program-build-options                    :string)
  (:program-build-log                        :string)
  (:program-binary-type                      %ocl:program-binary-type)
  #+opencl-2.0
  (:program-build-global-variable-total-size %ocl:size-t))

(define-info-getter get-program-info (program param) (%ocl:program-info)
  (:program-reference-count %ocl:uint)
  (:program-context         %ocl:context)
  (:program-num-devices     %ocl:uint)
  (:program-devices         %ocl:device-id :array t)
  (:program-source          :string)
  (:program-binary-sizes    %ocl:size-t :array t)
  #+opencl-1.2
  (:program-kernel-names    :string)
  #+opencl-1.2
  (:program-num-kernels     %ocl:size-t)
  (:program-binaries
   nil
   :form
   ;; fixme: test this...
   (let* ((sizes (get-program-info program :program-binary-sizes))
          (total-size (reduce #'+ sizes)))
     (with-foreign-pointer (buffer total-size)
       (with-foreign-object (pointers '(:pointer :void) (length sizes))
         (loop for j = 0 then (+ size j)
               for size in sizes
               for i from 0
               do (setf (mem-aref pointers :pointer i) (inc-pointer buffer j)))
         (%ocl/e:get-program-info program :program-binaries
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

(define-info-getter get-sampler-info (sampler param) (%ocl:sampler-info)
  (:sampler-reference-count   %ocl:uint)
  (:sampler-context           %ocl:context)
  (:sampler-normalized-coords %ocl:bool)
  (:sampler-addressing-mode   %ocl:addressing-mode)
  (:sampler-filter-mode       %ocl:filter-mode)
  ;; extensions, not included
  ;; (:sampler-lod-max %ocl:)
  ;; (:sampler-lod-min %ocl:)
  ;; (:sampler-mip-filter-mode %ocl:)
  )



