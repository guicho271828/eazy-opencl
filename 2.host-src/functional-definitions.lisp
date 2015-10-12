(in-package :eazy-opencl.host)


;;; info-getter definitions (alphabetical)

;; "returns specified info about a platform-id
;;  param = :profile, :version, :name, :vendor, :extensions"
(define-info-getter get-command-queue-info (command-queue param) (%cl/e:get-command-queue-info %cl:command-queue-info)
  (:queue-context         %cl:context)
  (:queue-device          %cl:device-id)
  (:queue-reference-count %cl:uint)
  (:queue-properties      %cl:command-queue-properties)
  (:queue-size            %cl:uint))

(define-info-getter get-context-info (context param) (%cl/e:get-context-info %cl:context-info)
 (:context-reference-count %cl:uint)
 (:context-devices         %cl:device-id :array t)
 (:context-properties      %cl:context-properties :array t :plist t))
;; fixme : support plist stuff: alternating enum/value pairs terminated by a single 0

(define-info-getter get-device-info (device-id param) (%cl/e:get-device-info %cl:device-info)
  (:device-type                          %cl:device-type)
  (:device-vendor-id                     %cl:uint)
  (:device-max-compute-units             %cl:uint)
  (:device-max-work-item-dimensions      %cl:uint)
  (:device-max-work-item-sizes           %cl:size-t :querysize :max-work-item-dimensions)
  (:device-max-work-group-size           %cl:size-t)
  (:device-preferred-vector-width-char   %cl:uint)
  (:device-preferred-vector-width-short  %cl:uint)
  (:device-preferred-vector-width-int    %cl:uint)
  (:device-preferred-vector-width-long   %cl:uint)
  (:device-preferred-vector-width-float  %cl:uint)
  (:device-preferred-vector-width-double %cl:uint)
  (:device-max-clock-frequency           %cl:uint)
  (:device-address-bits                  %cl:uint)
  (:device-max-read-image-args           %cl:uint)
  (:device-max-write-image-args          %cl:uint)
  (:device-max-mem-alloc-size            %cl:ulong)
  (:device-image2d-max-width             %cl:size-t)
  (:device-image2d-max-height            %cl:size-t)
  (:device-image3d-max-width             %cl:size-t)
  (:device-image3d-max-height            %cl:size-t)
  (:device-image3d-max-depth             %cl:size-t)
  (:device-image-support                 %cl:bool)
  (:device-max-parameter-size            %cl:size-t)
  (:device-max-samplers                  %cl:uint)
  (:device-mem-base-addr-align           %cl:uint)
  (:device-min-data-type-align-size      %cl:uint)
  (:device-single-fp-config              %cl:device-fp-config)
  (:device-global-mem-cache-type         %cl:device-mem-cache-type)
  (:device-global-mem-cacheline-size     %cl:uint)
  (:device-global-mem-cache-size         %cl:ulong)
  (:device-global-mem-size               %cl:ulong)
  (:device-max-constant-buffer-size      %cl:ulong)
  (:device-max-constant-args             %cl:uint)
  (:device-local-mem-type                %cl:device-local-mem-type)
  (:device-local-mem-size                %cl:ulong)
  (:device-error-correction-support      %cl:bool)
  (:device-profiling-timer-resolution    %cl:size-t)
  (:device-endian-little                 %cl:bool)
  (:device-available                     %cl:bool)
  (:device-compiler-available            %cl:bool)
  (:device-execution-capabilities        %cl:device-exec-capabilities)
  (:device-queue-properties              %cl:command-queue-properties)
  (:device-name           :string)
  (:device-vendor         :string)
  (:driver-version        :string)
  (:device-profile        :string)
  (:device-version        :string)
  (:device-extensions     :string)
  (:device-platform                      %cl:platform-id)
  ;; opencl 1.1
  (:device-host-unified-memory           %cl:bool)
  (:device-preferred-vector-width-half   %cl:uint)
  (:device-native-vector-width-half      %cl:uint)
  (:device-native-vector-width-char      %cl:uint)
  (:device-native-vector-width-short     %cl:uint)
  (:device-native-vector-width-int       %cl:uint)
  (:device-native-vector-width-long      %cl:uint)
  (:device-native-vector-width-float     %cl:uint)
  (:device-native-vector-width-double    %cl:uint)
  (:device-opencl-c-version :string))

(define-info-getter get-event-info (event param) (%cl/e:get-event-info %cl:event-info)
  (:event-command-queue            %cl:command-queue)
  #+opencl-1.1
  (:event-context                  %cl:context)
  (:event-command-type             %cl:command-type)
  (:event-command-execution-status %cl:int)
  (:event-reference-count          %cl:uint))

(define-info-getter get-event-profiling-info (event param) (%cl/e:get-event-profiling-info %cl:profiling-info)
  (:profiling-command-queued %cl:ulong)
  (:profiling-command-submit %cl:ulong)
  (:profiling-command-start  %cl:ulong)
  (:profiling-command-end    %cl:ulong)
  #+opencl-2.0
  (:profiling-command-complete %cl:ulong))

(define-info-getter get-image-info (image param) (%cl/e:get-image-info %cl:image-info)
  (:image-format       %cl:image-format)
  (:image-element-size %cl:size-t)
  (:image-row-pitch    %cl:size-t)
  (:image-slice-pitch  %cl:size-t)
  (:image-width        %cl:size-t)
  (:image-height       %cl:size-t)
  (:image-depth        %cl:size-t))

(define-info-getter get-kernel-info (kernel param) (%cl/e:get-kernel-info %cl:kernel-info)
  (:kernel-function-name :string)
  (:kernel-num-args        %cl:uint)
  (:kernel-reference-count %cl:uint)
  (:kernel-context         %cl:context)
  (:kernel-program         %cl:program))

#+opencl-2.0
(define-info-getter get-kernel-exec-info (kernel param) (%cl/e:get-kernel-exec-info %cl:kernel-exec-info)
  (:kernel-exec-info-svm-ptrs (:pointer :void) :array t)
  (:kernel-exec-info-svm-fine-grain-system %cl:bool))

(define-info-getter get-kernel-work-group-info (kernel device param) (%cl/e:get-kernel-work-group-info %cl:kernel-work-group-info)
  (:kernel-global-work-size                   %cl:size-t :fixedsize 3)
  (:kernel-work-group-size                    %cl:size-t)
  (:kernel-compile-work-group-size            %cl:size-t :fixedsize 3)
  (:kernel-local-mem-size          %cl:ulong)
  (:kernel-preferred-work-group-size-multiple %cl:ulong)
  (:kernel-private-mem-size                   %cl:ulong))


(define-info-getter get-mem-object-info (memobj param) (%cl/e:get-mem-object-info %cl:mem-info)
  (:mem-type                 %cl:mem-object-type)
  (:mem-flags                %cl:mem-flags)
  (:mem-size                 %cl:size-t)
  (:mem-host-ptr             (:pointer :void))
  (:mem-map-count            %cl:uint)
  (:mem-reference-count      %cl:uint)
  (:mem-context              %cl:context)
  ;; 1.1
  (:mem-associated-memobject %cl:mem)
  (:mem-offset               %cl:size-t)
  (:mem-uses-svm-pointer  %cl:bool))

#+opencl-2.0
(define-info-getter get-pipe-info (pipe param) (%cl/e:get-pipe-info %cl:pipe-info)
  (:pipe-packet-size %cl:uint)
  (:pipe-max-packets %cl:uint))

(define-info-getter get-platform-info (platform-id param) (%cl/e:get-platform-info %cl:platform-info)
  (:platform-profile        :string)
  (:platform-version        :string)
  (:platform-name           :string)
  (:platform-vendor         :string)
  (:platform-extensions     :string)
  (:platform-icd-suffix-khr :string))

(define-info-getter get-program-build-info (program device param) (%cl/e:get-program-build-info %cl:program-build-info)
  (:program-build-status  %cl:build-status)
  (:program-build-options                    :string)
  (:program-build-log                        :string)
  (:program-binary-type                      :string)
  (:program-build-global-variable-total-size :string))

(define-info-getter get-program-info (program param) (%cl/e:get-program-info %cl:program-info)
  (:program-reference-count %cl:uint)
  (:program-context         %cl:context)
  (:program-num-devices     %cl:uint)
  (:program-devices         %cl:device-id :array t)
  (:program-source          :string)
  (:program-binary-sizes    %cl:size-t :array t)
  (:program-binaries
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

(define-info-getter get-sampler-info (sampler param) (%cl/e:get-sampler-info %cl:sampler-info)
  (:sampler-reference-count   %cl:uint)
  (:sampler-context           %cl:context)
  (:sampler-normalized-coords %cl:bool)
  (:sampler-addressing-mode   %cl:addressing-mode)
  (:sampler-filter-mode       %cl:filter-mode))



