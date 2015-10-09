;;;; based on files with following copyright:
;;;;
;;;; * Copyright (c) 2008-2009 The Khronos Group Inc.
;;;; *
;;;; * Permission is hereby granted, free of charge, to any person obtaining a
;;;; * copy of this software and/or associated documentation files (the
;;;; * "Materials"), to deal in the Materials without restriction, including
;;;; * without limitation the rights to use, copy, modify, merge, publish,
;;;; * distribute, sublicense, and/or sell copies of the Materials, and to
;;;; * permit persons to whom the Materials are furnished to do so, subject to
;;;; * the following conditions:
;;;; *
;;;; * The above copyright notice and this permission notice shall be included
;;;; * in all copies or substantial portions of the Materials.
;;;; *
;;;; * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.

(cl:in-package #:eazy-opencl.bindings)

(cffi::defctype uint32-t :uint32)

(cffi::defctype uint uint32-t)

(cffi::defctype uint64-t :uint64)

(cffi::defctype ulong uint64-t)

(cffi::defctype bitfield ulong)

(cffi::defctype int32-t :int32)

(cffi::defctype int int32-t)


;;(cffi::defctype intptr-t :intptr)

(defcenum error-code
  (:SUCCESS 0)
  (:DEVICE-NOT-FOUND -1)
  (:DEVICE-NOT-AVAILABLE -2)
  (:COMPILER-NOT-AVAILABLE -3)
  (:MEM-OBJECT-ALLOCATION-FAILURE -4)
  (:OUT-OF-RESOURCES -5)
  (:OUT-OF-HOST-MEMORY -6)
  (:PROFILING-INFO-NOT-AVAILABLE -7)
  (:MEM-COPY-OVERLAP -8)
  (:IMAGE-FORMAT-MISMATCH -9)
  (:IMAGE-FORMAT-NOT-SUPPORTED -10)
  (:BUILD-PROGRAM-FAILURE -11)
  (:MAP-FAILURE -12)
  (:misaligned-sub-buffer-offset -13) ;; 1.1
  (:exec-status-error-for-events-in-wait-list -14) ;; 1.1
  (:INVALID-VALUE -30)
  (:INVALID-DEVICE-TYPE -31)
  (:INVALID-PLATFORM -32)
  (:INVALID-DEVICE -33)
  (:INVALID-CONTEXT -34)
  (:INVALID-QUEUE-PROPERTIES -35)
  (:INVALID-COMMAND-QUEUE -36)
  (:INVALID-HOST-PTR -37)
  (:INVALID-MEM-OBJECT -38)
  (:INVALID-IMAGE-FORMAT-DESCRIPTOR -39)
  (:INVALID-IMAGE-SIZE -40)
  (:INVALID-SAMPLER -41)
  (:INVALID-BINARY -42)
  (:INVALID-BUILD-OPTIONS -43)
  (:INVALID-PROGRAM -44)
  (:INVALID-PROGRAM-EXECUTABLE -45)
  (:INVALID-KERNEL-NAME -46)
  (:INVALID-KERNEL-DEFINITION -47)
  (:INVALID-KERNEL -48)
  (:INVALID-ARG-INDEX -49)
  (:INVALID-ARG-VALUE -50)
  (:INVALID-ARG-SIZE -51)
  (:INVALID-KERNEL-ARGS -52)
  (:INVALID-WORK-DIMENSION -53)
  (:INVALID-WORK-GROUP-SIZE -54)
  (:INVALID-WORK-ITEM-SIZE -55)
  (:INVALID-GLOBAL-OFFSET -56)
  (:INVALID-EVENT-WAIT-LIST -57)
  (:INVALID-EVENT -58)
  (:INVALID-OPERATION -59)
  (:INVALID-GL-OBJECT -60)
  (:INVALID-BUFFER-SIZE -61)
  (:INVALID-MIP-LEVEL -62)
  (:INVALID-GLOBAL-WORK-SIZE -63)
  ;; 1.1
  (:invalid-property -64)
  ;; cl_khr_gl_sharing
  (:invalid-sharegroup-reference-khr -1000)
  ;; cl_khr_icd
  (:platform-not-found-khr -1001))

(defcenum (platform-info uint)
  (:profile        #x0900)
  (:version        #x0901)
  (:name           #x0902)
  (:vendor         #x0903)
  (:extensions     #x0904)
  ;; cl_khr_icd
  (:icd-suffix-khr #x0920))

(defbitfield (device-type bitfield)
  (:DEFAULT                      1)
  (:CPU                          2)
  (:GPU                          4)
  (:ACCELERATOR                  8)
  (:ALL                 #xFFFFFFFF))

(defcenum (device-info uint)
  (:type                              #x1000)
  (:vendor-id                         #x1001)
  (:max-compute-units                 #x1002)
  (:max-work-item-dimensions          #x1003)
  (:max-work-group-size               #x1004)
  (:max-work-item-sizes               #x1005)
  (:preferred-vector-width-char       #x1006)
  (:preferred-vector-width-short      #x1007)
  (:preferred-vector-width-int        #x1008)
  (:preferred-vector-width-long       #x1009)
  (:preferred-vector-width-float      #x100a)
  (:preferred-vector-width-double     #x100b)
  (:max-clock-frequency               #x100c)
  (:address-bits                      #x100d)
  (:max-read-image-args               #x100e)
  (:max-write-image-args              #x100f)
  (:max-mem-alloc-size                #x1010)
  (:image2d-max-width                 #x1011)
  (:image2d-max-height                #x1012)
  (:image3d-max-width                 #x1013)
  (:image3d-max-height                #x1014)
  (:image3d-max-depth                 #x1015)
  (:image-support                     #x1016)
  (:max-parameter-size                #x1017)
  (:max-samplers                      #x1018)
  (:mem-base-addr-align               #x1019)
  (:min-data-type-align-size          #x101a)
  (:single-fp-config                  #x101b)
  (:global-mem-cache-type             #x101c)
  (:global-mem-cacheline-size         #x101d)
  (:global-mem-cache-size             #x101e)
  (:global-mem-size                   #x101f)
  (:max-constant-buffer-size          #x1020)
  (:max-constant-args                 #x1021)
  (:local-mem-type                    #x1022)
  (:local-mem-size                    #x1023)
  (:error-correction-support          #x1024)
  (:profiling-timer-resolution        #x1025)
  (:endian-little                     #x1026)
  (:available                         #x1027)
  (:compiler-available                #x1028)
  (:execution-capabilities            #x1029)
  (:queue-properties                  #x102a)
  (:name                              #x102b)
  (:vendor                            #x102c)
  (:driver-version                    #x102d)
  (:profile                           #x102e)
  (:version                           #x102f)
  (:extensions                        #x1030)
  (:platform                          #x1031)
  ;;/* 0x1032 reserved for CL_DEVICE_DOUBLE_FP_CONFIG */
  ;;/* 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG */
   (:PREFERRED-VECTOR-WIDTH-HALF       #x1034) ;;1.1
   (:HOST-UNIFIED-MEMORY               #x1035) ;;1.1
   (:NATIVE-VECTOR-WIDTH-CHAR          #x1036) ;;1.1
   (:NATIVE-VECTOR-WIDTH-SHORT         #x1037) ;;1.1
   (:NATIVE-VECTOR-WIDTH-INT           #x1038) ;;1.1
   (:NATIVE-VECTOR-WIDTH-LONG          #x1039) ;;1.1
   (:NATIVE-VECTOR-WIDTH-FLOAT         #x103A) ;;1.1
   (:NATIVE-VECTOR-WIDTH-DOUBLE        #x103B) ;;1.1
   (:NATIVE-VECTOR-WIDTH-HALF          #x103C) ;;1.1
   (:OPENCL-C-VERSION                  #x103D)) ;;1.1

(defbitfield (device-fp-config bitfield)
  (:DENORM                                #.(cl:ash 1 0))
  (:INF-NAN                               #.(cl:ash 1 1))
  (:ROUND-TO-NEAREST                      #.(cl:ash 1 2))
  (:ROUND-TO-ZERO                         #.(cl:ash 1 3))
  (:ROUND-TO-INF                          #.(cl:ash 1 4))
  (:FMA                                   #.(cl:ash 1 5))
  (:FP-SOFT-FLOAT                         #.(cl:ash 1 6))) ;; 1.1


(defcenum (device-mem-cache-type uint)
  (:NONE                                     #x0)
  (:READ-ONLY-CACHE                          #x1)
  (:READ-WRITE-CACHE                         #x2))

(defcenum (device-local-mem-type uint)
  (:LOCAL                                    #x1)
  (:GLOBAL                                   #x2))

(defbitfield (device-exec-capabilities bitfield)
  (:KERNEL                              #.(cl:ash 1 0))
  (:NATIVE-KERNEL                       #.(cl:ash 1 1)))

(defbitfield (command-queue-properties bitfield)
  (:OUT-OF-ORDER-EXEC-MODE-ENABLE      #.(cl:ash 1 0))
  (:PROFILING-ENABLE                   #.(cl:ash 1 1)))

(defcenum (context-info uint)
  (:REFERENCE-COUNT                  #x1080)
  (:DEVICES                          #x1081)
  (:PROPERTIES                       #x1082)
  (:context-num-devices              #x1083)) ;; 1.1

;(cffi::defctype cl-context-properties intptr-t) ?
;; intptr-t in C headers, but easier to use as unsigned, so :uintptr here
(defcenum (context-properties :uintptr)
  (:PLATFORM                         #x1084)
  ;; cl_khr_gl_sharing
  (:gl-context-khr                   #x2008)
  (:egl-display-khr                  #x2009)
  (:glx-display-khr                  #x200a)
  (:wgl-hdc-khr                      #x200b)
  (:cgl-sharegroup-khr               #x200c))

(defcenum (command-queue-info uint)
  (:CONTEXT                            #x1090)
  (:DEVICE                             #x1091)
  (:REFERENCE-COUNT                    #x1092)
  (:PROPERTIES                         #x1093))

(defbitfield (mem-flags bitfield)
  (:READ-WRITE                           #.(cl:ash 1 0))
  (:WRITE-ONLY                           #.(cl:ash 1 1))
  (:READ-ONLY                            #.(cl:ash 1 2))
  (:USE-HOST-PTR                         #.(cl:ash 1 3))
  (:ALLOC-HOST-PTR                       #.(cl:ash 1 4))
  (:COPY-HOST-PTR                        #.(cl:ash 1 5)))

(defcenum (channel-order uint)
  (:R                                        #x10B0)
  (:A                                        #x10B1)
  (:RG                                       #x10B2)
  (:RA                                       #x10B3)
  (:RGB                                      #x10B4)
  (:RGBA                                     #x10B5)
  (:BGRA                                     #x10B6)
  (:ARGB                                     #x10B7)
  (:INTENSITY                                #x10B8)
  (:LUMINANCE                                #x10B9)
  (:rx                                       #x10BA) ;;1.1
  (:rgx                                      #x10BB) ;;1.1
  (:rgbx                                     #x10BC)) ;;1.1

(defcenum (channel-type uint)
  (:SNORM-INT8                               #x10D0)
  (:SNORM-INT16                              #x10D1)
  (:UNORM-INT8                               #x10D2)
  (:UNORM-INT16                              #x10D3)
  (:UNORM-SHORT-565                          #x10D4)
  (:UNORM-SHORT-555                          #x10D5)
  (:UNORM-INT-101010                         #x10D6)
  (:SIGNED-INT8                              #x10D7)
  (:SIGNED-INT16                             #x10D8)
  (:SIGNED-INT32                             #x10D9)
  (:UNSIGNED-INT8                            #x10DA)
  (:UNSIGNED-INT16                           #x10DB)
  (:UNSIGNED-INT32                           #x10DC)
  (:HALF-FLOAT                               #x10DD)
  (:FLOAT                                    #x10DE))

(defcenum (mem-object-type uint)
  (:BUFFER                        #x10F0)
  (:IMAGE2D                       #x10F1)
  (:IMAGE3D                       #x10F2))

(defcenum (mem-info uint)
  (:TYPE                                 #x1100)
  (:FLAGS                                #x1101)
  (:SIZE                                 #x1102)
  (:HOST-PTR                             #x1103)
  (:MAP-COUNT                            #x1104)
  (:REFERENCE-COUNT                      #x1105)
  (:CONTEXT                              #x1106)
  (:associated-memobject                 #x1107) ;;1.1
  (:offset                               #x1108));;1.1

(defcenum (image-info uint)
  (:FORMAT                             #x1110)
  (:ELEMENT-SIZE                       #x1111)
  (:ROW-PITCH                          #x1112)
  (:SLICE-PITCH                        #x1113)
  (:WIDTH                              #x1114)
  (:HEIGHT                             #x1115)
  (:DEPTH                              #x1116))

(defcenum (addressing-mode uint)
  (:NONE                             #x1130)
  (:CLAMP-TO-EDGE                    #x1131)
  (:CLAMP                            #x1132)
  (:REPEAT                           #x1133)
  (:mirrored-repeat                  #x1134)) ;;1.1

(defcenum (filter-mode uint)
  (:NEAREST                           #x1140)
  (:LINEAR                            #x1141))

(defcenum (sampler-info uint)
  (:REFERENCE-COUNT                  #x1150)
  (:CONTEXT                          #x1151)
  (:NORMALIZED-COORDS                #x1152)
  (:ADDRESSING-MODE                  #x1153)
  (:FILTER-MODE                      #x1154))

(defbitfield (map-flags bitfield)
  (:READ                                 #.(cl:ash 1 0))
  (:WRITE                                #.(cl:ash 1 1)))

(defcenum (program-info uint)
  (:REFERENCE-COUNT                  #x1160)
  (:CONTEXT                          #x1161)
  (:NUM-DEVICES                      #x1162)
  (:DEVICES                          #x1163)
  (:SOURCE                           #x1164)
  (:BINARY-SIZES                     #x1165)
  (:BINARIES                         #x1166))

(defcenum (program-build-info uint)
  (:STATUS                     #x1181)
  (:OPTIONS                    #x1182)
  (:LOG                        #x1183))

(defcenum (build-status int)
  (:SUCCESS                            0)
  (:NONE                               -1)
  (:ERROR                              -2)
  (:IN-PROGRESS                        -3))

(defcenum (kernel-info uint)
  (:FUNCTION-NAME                     #x1190)
  (:NUM-ARGS                          #x1191)
  (:REFERENCE-COUNT                   #x1192)
  (:CONTEXT                           #x1193)
  (:PROGRAM                           #x1194))

(defcenum (kernel-work-group-info uint)
  (:WORK-GROUP-SIZE                   #x11B0)
  (:COMPILE-WORK-GROUP-SIZE           #x11B1)
  (:LOCAL-MEM-SIZE                    #x11B2)
  (:preferred-work-group-size-multiple #x11b3) ;;1.1
  (:private-mem-size                  #x11b4)) ;;1.1

(defcenum (event-info uint)
  (:COMMAND-QUEUE                      #x11D0)
  (:COMMAND-TYPE                       #x11D1)
  (:REFERENCE-COUNT                    #x11D2)
  (:COMMAND-EXECUTION-STATUS           #x11D3)
  (:event-context                      #x11d4)) ;;1.1

(defcenum (command-type uint)
  (:NDRANGE-KERNEL                   #x11F0)
  (:TASK                             #x11F1)
  (:NATIVE-KERNEL                    #x11F2)
  (:READ-BUFFER                      #x11F3)
  (:WRITE-BUFFER                     #x11F4)
  (:COPY-BUFFER                      #x11F5)
  (:READ-IMAGE                       #x11F6)
  (:WRITE-IMAGE                      #x11F7)
  (:COPY-IMAGE                       #x11F8)
  (:COPY-IMAGE-TO-BUFFER             #x11F9)
  (:COPY-BUFFER-TO-IMAGE             #x11FA)
  (:MAP-BUFFER                       #x11FB)
  (:MAP-IMAGE                        #x11FC)
  (:UNMAP-MEM-OBJECT                 #x11FD)
  (:MARKER                           #x11FE)
  (:ACQUIRE-GL-OBJECTS               #x11FF)
  (:RELEASE-GL-OBJECTS               #x1200)
  (:read-buffer-rect                 #x1201) ;;1.1
  (:write-buffer-rect                #x1202) ;;1.1
  (:copy-buffer-rect                 #x1203) ;;1.1
  (:user                             #x1204));;1.1

(defcenum command-execution-status
  (:COMPLETE                                 #x0)
  (:RUNNING                                  #x1)
  (:SUBMITTED                                #x2)
  (:QUEUED                                   #x3))

(defcenum (profiling-info uint)
  (:QUEUED                 #x1280)
  (:SUBMIT                 #x1281)
  (:START                  #x1282)
  (:END                    #x1283))




(cffi:defcstruct _image-format
  (image-channel-order channel-order)
  (image-channel-data-type channel-type))

(cffi::defctype image-format _image-format)


(cffi:defcstruct _program
  )

(cffi::defctype program :pointer)

(cffi:defcstruct _kernel
  )

(cffi::defctype kernel :pointer)


(cffi:defcstruct _context
  )

(cffi::defctype context :pointer)

(cffi:defcstruct _device-id
  )

(cffi::defctype device-id :pointer)

(cffi:defcstruct _mem
  )

(cffi::defctype mem :pointer)

(cffi::defctype size-t :unsigned-long)


;(cffi::defctype ulong-16 uint64-t :count 16)

(cffi:defcstruct _command-queue
  )

(cffi::defctype command-queue :pointer)

(cffi::defctype bool :boolean)

(cffi:defcstruct _event
  )

(cffi::defctype event :pointer)

(cffi::defctype uint8-t :unsigned-char)

;(cffi::defctype uchar-16 uint8-t :count 16)

(cffi::defctype int16-t :short)

;(cffi::defctype short-8 int16-t :count 8)

(cffi:defcstruct _sampler
  )

(cffi::defctype sampler :pointer)


(cffi:defcstruct _platform-id
  )

(cffi::defctype platform-id :pointer)

;(cffi::defctype uint-16 uint-32-t :count 16)

(cffi::defctype int64-t :long)

;(cffi::defctype long-2 int64-t :count 2)

;(cffi::defctype long-4 int64-t :count 4)

;;(cffi::defctype long-8 int64-t :count 8)

;;(cffi::defctype short-16 int16-t :count 16)

;;(cffi::defctype long-16 int64-t :count 16)

;;(cffi::defctype float-16 :float :count 16)

;;(cffi::defctype int-2 int32-t :count 2)

;;(cffi::defctype int-4 int32-t :count 4)

(cffi::defctype long int64-t)

(cffi::defctype uint16-t :unsigned-short)

(cffi::defctype half uint16-t)

(cffi::defctype short int16-t)

;;(cffi::defctype uint-2 uint32-t :count 2)

;;(cffi::defctype uint-4 uint32-t :count 4)

;;(cffi::defctype uint-8 uint32-t :count 8)

;;(cffi::defctype int-16 int32-t :count 16)

(cffi::defctype int8-t :char)

(cffi::defctype char int8-t)

;;(cffi::defctype uchar-2 uint8-t :count 2)

;;(cffi::defctype uchar-4 uint8-t :count 4)

;;(cffi::defctype uchar-8 uint8-t :count 8)

;;(cffi::defctype int-8 int32-t :count 8)

(cffi::defctype device-address-info bitfield)

;;(cffi::defctype ushort-16 uint16-t :count 16)

;;(cffi::defctype double-16 :double :count 16)

;;(cffi::defctype char-2 int8-t :count 2)

;;(cffi::defctype char-4 int8-t :count 4)

;;(cffi::defctype char-8 int8-t :count 8)

;;(cffi::defctype short-2 int16-t :count 2)

;;(cffi::defctype short-4 int16-t :count 4)

(cffi::defctype uchar uint8-t)

;;(cffi::defctype ulong-2 uint64-t :count 2)

;;(cffi::defctype ulong-4 uint64-t :count 4)

;;(cffi::defctype ulong-8 uint64-t :count 8)

(cffi::defctype double :double)

;;(cffi::defctype double-2 :double :count 2)

;;(cffi::defctype double-4 :double :count 4)

;;(cffi::defctype double-8 :double :count 8)

;;(cffi::defctype float-2 :float :count 2)

;;(cffi::defctype float-4 :float :count 4)

;;(cffi::defctype ushort-2 uint16-t :count 2)

;;(cffi::defctype ushort-4 uint16-t :count 4)

;;(cffi::defctype float-8 :float :count 8)

;;(cffi::defctype ushort-8 uint16-t :count 8)

(cffi::defctype ushort uint16-t)

(cffi::defctype float :float)

;;(cffi::defctype char-16 int8-t :count 16)


;;(cffi::defctype uint-16-t :unsigned-short)

;;(cffi:defcstruct _buffer-region
;;  (origin size-t)
;;  (size size-t))
;;(cffi::defctype buffer-region _buffer-region)

(defcenum (buffer-create-type uint)
  (:region #x1220))


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
