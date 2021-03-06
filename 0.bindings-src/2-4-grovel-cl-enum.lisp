;;; groveller file

(in-package #:eazy-opencl.grovel)
#+darwin
(include "OpenCL/cl.h")
#-darwin
(include "CL/cl.h")

#+darwin
(include "OpenCL/cl_ext.h")
#-darwin
(include "CL/cl_ext.h")

(constant (#.(lispify "FALSE") "CL_FALSE"))
(constant (#.(lispify "TRUE") "CL_TRUE"))
(constant (#.(lispify "BLOCKING") "CL_BLOCKING"))
(constant (#.(lispify "NON_BLOCKING") "CL_NON_BLOCKING"))


;;; enums

(constantenum (--error-code :base-type int) ; from CL_INT
              (#.(lispify-k-pair "CL_SUCCESS"))
              (#.(lispify-k-pair "CL_DEVICE_NOT_FOUND"))
              (#.(lispify-k-pair "CL_DEVICE_NOT_AVAILABLE"))
              (#.(lispify-k-pair "CL_COMPILER_NOT_AVAILABLE"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_ALLOCATION_FAILURE"))
              (#.(lispify-k-pair "CL_OUT_OF_RESOURCES"))
              (#.(lispify-k-pair "CL_OUT_OF_HOST_MEMORY"))
              (#.(lispify-k-pair "CL_PROFILING_INFO_NOT_AVAILABLE"))
              (#.(lispify-k-pair "CL_MEM_COPY_OVERLAP"))
              (#.(lispify-k-pair "CL_IMAGE_FORMAT_MISMATCH"))
              (#.(lispify-k-pair "CL_IMAGE_FORMAT_NOT_SUPPORTED"))
              (#.(lispify-k-pair "CL_BUILD_PROGRAM_FAILURE"))
              (#.(lispify-k-pair "CL_MAP_FAILURE"))
              (#.(lispify-k-pair "CL_MISALIGNED_SUB_BUFFER_OFFSET"))
              (#.(lispify-k-pair "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST"))
              (#.(lispify-k-pair "CL_COMPILE_PROGRAM_FAILURE"))
              (#.(lispify-k-pair "CL_LINKER_NOT_AVAILABLE"))
              (#.(lispify-k-pair "CL_LINK_PROGRAM_FAILURE"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_FAILED"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_INFO_NOT_AVAILABLE"))
              (#.(lispify-k-pair "CL_INVALID_VALUE"))
              (#.(lispify-k-pair "CL_INVALID_DEVICE_TYPE"))
              (#.(lispify-k-pair "CL_INVALID_PLATFORM"))
              (#.(lispify-k-pair "CL_INVALID_DEVICE"))
              (#.(lispify-k-pair "CL_INVALID_CONTEXT"))
              (#.(lispify-k-pair "CL_INVALID_QUEUE_PROPERTIES"))
              (#.(lispify-k-pair "CL_INVALID_COMMAND_QUEUE"))
              (#.(lispify-k-pair "CL_INVALID_HOST_PTR"))
              (#.(lispify-k-pair "CL_INVALID_MEM_OBJECT"))
              (#.(lispify-k-pair "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR"))
              (#.(lispify-k-pair "CL_INVALID_IMAGE_SIZE"))
              (#.(lispify-k-pair "CL_INVALID_SAMPLER"))
              (#.(lispify-k-pair "CL_INVALID_BINARY"))
              (#.(lispify-k-pair "CL_INVALID_BUILD_OPTIONS"))
              (#.(lispify-k-pair "CL_INVALID_PROGRAM"))
              (#.(lispify-k-pair "CL_INVALID_PROGRAM_EXECUTABLE"))
              (#.(lispify-k-pair "CL_INVALID_KERNEL_NAME"))
              (#.(lispify-k-pair "CL_INVALID_KERNEL_DEFINITION"))
              (#.(lispify-k-pair "CL_INVALID_KERNEL"))
              (#.(lispify-k-pair "CL_INVALID_ARG_INDEX"))
              (#.(lispify-k-pair "CL_INVALID_ARG_VALUE"))
              (#.(lispify-k-pair "CL_INVALID_ARG_SIZE"))
              (#.(lispify-k-pair "CL_INVALID_KERNEL_ARGS"))
              (#.(lispify-k-pair "CL_INVALID_WORK_DIMENSION"))
              (#.(lispify-k-pair "CL_INVALID_WORK_GROUP_SIZE"))
              (#.(lispify-k-pair "CL_INVALID_WORK_ITEM_SIZE"))
              (#.(lispify-k-pair "CL_INVALID_GLOBAL_OFFSET"))
              (#.(lispify-k-pair "CL_INVALID_EVENT_WAIT_LIST"))
              (#.(lispify-k-pair "CL_INVALID_EVENT"))
              (#.(lispify-k-pair "CL_INVALID_OPERATION"))
              (#.(lispify-k-pair "CL_INVALID_GL_OBJECT"))
              (#.(lispify-k-pair "CL_INVALID_BUFFER_SIZE"))
              (#.(lispify-k-pair "CL_INVALID_MIP_LEVEL"))
              (#.(lispify-k-pair "CL_INVALID_GLOBAL_WORK_SIZE"))
              (#.(lispify-k-pair "CL_INVALID_PROPERTY"))
              (#.(lispify-k-pair "CL_INVALID_IMAGE_DESCRIPTOR"))
              (#.(lispify-k-pair "CL_INVALID_COMPILER_OPTIONS"))
              (#.(lispify-k-pair "CL_INVALID_LINKER_OPTIONS"))
              (#.(lispify-k-pair "CL_INVALID_DEVICE_PARTITION_COUNT")) 
              #+opencl-2.0
              (#.(lispify-k-pair "CL_INVALID_PIPE_SIZE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_INVALID_DEVICE_QUEUE"))
              ;; extension
              (#.(lispify-k-pair "CL_PLATFORM_NOT_FOUND_KHR")))

(constantenum (#.(lispify "platform_info") :base-type #.(lispify "__platform_info"))
              (#.(lispify-k-pair "CL_PLATFORM_PROFILE"))
              (#.(lispify-k-pair "CL_PLATFORM_VERSION"))
              (#.(lispify-k-pair "CL_PLATFORM_NAME"))
              (#.(lispify-k-pair "CL_PLATFORM_VENDOR"))
              (#.(lispify-k-pair "CL_PLATFORM_EXTENSIONS"))
              ;; extension
              (#.(lispify-k-pair "CL_PLATFORM_ICD_SUFFIX_KHR")))

(bitfield (#.(lispify "device_type") :base-type #.(lispify "__device_type"))
          (#.(lispify-k-pair "CL_DEVICE_TYPE_DEFAULT"))
          (#.(lispify-k-pair "CL_DEVICE_TYPE_CPU"))
          (#.(lispify-k-pair "CL_DEVICE_TYPE_GPU"))
          (#.(lispify-k-pair "CL_DEVICE_TYPE_ACCELERATOR"))
          (#.(lispify-k-pair "CL_DEVICE_TYPE_CUSTOM"))
          (#.(lispify-k-pair "CL_DEVICE_TYPE_ALL")))

(constantenum (#.(lispify "device_info") :base-type #.(lispify "__device_info"))
              (#.(lispify-k-pair "CL_DEVICE_TYPE"))
              (#.(lispify-k-pair "CL_DEVICE_VENDOR_ID"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_COMPUTE_UNITS"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_WORK_GROUP_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_WORK_ITEM_SIZES"))
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR"))
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT"))
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT"))
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG"))
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT"))
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_CLOCK_FREQUENCY"))
              (#.(lispify-k-pair "CL_DEVICE_ADDRESS_BITS"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_READ_IMAGE_ARGS"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_WRITE_IMAGE_ARGS"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_MEM_ALLOC_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE2D_MAX_WIDTH"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE2D_MAX_HEIGHT"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE3D_MAX_WIDTH"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE3D_MAX_HEIGHT"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE3D_MAX_DEPTH"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE_SUPPORT"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_PARAMETER_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_SAMPLERS"))
              (#.(lispify-k-pair "CL_DEVICE_MEM_BASE_ADDR_ALIGN"))
              ;; #-opencl-1.2
              (#.(lispify-k-pair "CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_SINGLE_FP_CONFIG"))
              (#.(lispify-k-pair "CL_DEVICE_GLOBAL_MEM_CACHE_TYPE"))
              (#.(lispify-k-pair "CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_GLOBAL_MEM_CACHE_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_GLOBAL_MEM_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_MAX_CONSTANT_ARGS"))
              (#.(lispify-k-pair "CL_DEVICE_LOCAL_MEM_TYPE"))
              (#.(lispify-k-pair "CL_DEVICE_LOCAL_MEM_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_ERROR_CORRECTION_SUPPORT"))
              (#.(lispify-k-pair "CL_DEVICE_PROFILING_TIMER_RESOLUTION"))
              (#.(lispify-k-pair "CL_DEVICE_ENDIAN_LITTLE"))
              (#.(lispify-k-pair "CL_DEVICE_AVAILABLE"))
              (#.(lispify-k-pair "CL_DEVICE_COMPILER_AVAILABLE"))
              (#.(lispify-k-pair "CL_DEVICE_EXECUTION_CAPABILITIES"))
              ;; #-opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_QUEUE_PROPERTIES"))
              ;; in opencl-2.0, CL_DEVICE_QUEUE_PROPERTIES is replaced by CL_DEVICE_QUEUE_ON_HOST_PROPERTIES
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_QUEUE_ON_HOST_PROPERTIES"))
              (#.(lispify-k-pair "CL_DEVICE_NAME"))
              (#.(lispify-k-pair "CL_DEVICE_VENDOR"))
              (#.(lispify-k-pair "CL_DRIVER_VERSION"))
              (#.(lispify-k-pair "CL_DEVICE_PROFILE"))
              (#.(lispify-k-pair "CL_DEVICE_VERSION"))
              (#.(lispify-k-pair "CL_DEVICE_EXTENSIONS"))
              (#.(lispify-k-pair "CL_DEVICE_PLATFORM"))
              (#.(lispify-k-pair "CL_DEVICE_DOUBLE_FP_CONFIG"))
              ;; 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF"))
              ;; #+(and opencl-1.1 (not opencl-2.0))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_HOST_UNIFIED_MEMORY"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_NATIVE_VECTOR_WIDTH_INT"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_DEVICE_OPENCL_C_VERSION"))
              (#.(lispify-k-pair "CL_DEVICE_LINKER_AVAILABLE"))
              (#.(lispify-k-pair "CL_DEVICE_BUILT_IN_KERNELS"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE_MAX_BUFFER_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE_MAX_ARRAY_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_PARENT_DEVICE"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_MAX_SUB_DEVICES"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_PROPERTIES"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_AFFINITY_DOMAIN"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_TYPE"))
              (#.(lispify-k-pair "CL_DEVICE_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_INTEROP_USER_SYNC"))
              (#.(lispify-k-pair "CL_DEVICE_PRINTF_BUFFER_SIZE"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE_PITCH_ALIGNMENT"))
              (#.(lispify-k-pair "CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_MAX_ON_DEVICE_QUEUES"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_MAX_ON_DEVICE_EVENTS"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_SVM_CAPABILITIES"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_MAX_PIPE_ARGS"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_PIPE_MAX_PACKET_SIZE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT")))

(bitfield (#.(lispify "device_fp_config") :base-type #.(lispify "__device_fp_config"))
          (#.(lispify-k-pair "CL_FP_DENORM"))
          (#.(lispify-k-pair "CL_FP_INF_NAN"))
          (#.(lispify-k-pair "CL_FP_ROUND_TO_NEAREST"))
          (#.(lispify-k-pair "CL_FP_ROUND_TO_ZERO"))
          (#.(lispify-k-pair "CL_FP_ROUND_TO_INF"))
          (#.(lispify-k-pair "CL_FP_FMA"))
          (#.(lispify-k-pair "CL_FP_SOFT_FLOAT"))
          (#.(lispify-k-pair "CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT")))

(constantenum (#.(lispify "device_mem_cache_type") :base-type #.(lispify "__device_mem_cache_type"))
              (#.(lispify-k-pair "CL_NONE"))
              (#.(lispify-k-pair "CL_READ_ONLY_CACHE"))
              (#.(lispify-k-pair "CL_READ_WRITE_CACHE")))

(constantenum (#.(lispify "device_local_mem_type") :base-type #.(lispify "__device_local_mem_type"))
              (#.(lispify-k-pair "CL_LOCAL"))
              (#.(lispify-k-pair "CL_GLOBAL")))

(bitfield (#.(lispify "device_exec_capabilities") :base-type #.(lispify "__device_exec_capabilities"))
          (#.(lispify-k-pair "CL_EXEC_KERNEL"))
          (#.(lispify-k-pair "CL_EXEC_NATIVE_KERNEL")))

(bitfield (#.(lispify "command_queue_properties") :base-type #.(lispify "__command_queue_properties"))
          (#.(lispify-k-pair "CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE"))
          (#.(lispify-k-pair "CL_QUEUE_PROFILING_ENABLE"))
          #+opencl-2.0
          (#.(lispify-k-pair "CL_QUEUE_ON_DEVICE"))
          #+opencl-2.0
          (#.(lispify-k-pair "CL_QUEUE_ON_DEVICE_DEFAULT")))

#+opencl-2.0
(constantenum (#.(lispify "queue_properties") :base-type #.(lispify "__queue_properties"))
              (#.(lispify-k-pair "CL_QUEUE_PROPERTIES"))
              (#.(lispify-k-pair "CL_QUEUE_SIZE")))

(constantenum (#.(lispify "context_info") :base-type #.(lispify "__context_info")) 
              (#.(lispify-k-pair "CL_CONTEXT_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_CONTEXT_DEVICES"))
              (#.(lispify-k-pair "CL_CONTEXT_PROPERTIES"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_CONTEXT_NUM_DEVICES")))

(constantenum (#.(lispify "context_properties") :base-type #.(lispify "__context_properties"))
              (#.(lispify-k-pair "CL_CONTEXT_PLATFORM"))
              (#.(lispify-k-pair "CL_CONTEXT_INTEROP_USER_SYNC")))

(constantenum (#.(lispify "device_partition_property") :base-type #.(lispify "__device_partition_property"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_EQUALLY"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_BY_COUNTS"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_BY_COUNTS_LIST_END"))
              (#.(lispify-k-pair "CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN")))

(bitfield (#.(lispify "device_affinity_domain") :base-type #.(lispify "__device_affinity_domain"))
          (#.(lispify-k-pair "CL_DEVICE_AFFINITY_DOMAIN_NUMA"))
          (#.(lispify-k-pair "CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE"))
          (#.(lispify-k-pair "CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE"))
          (#.(lispify-k-pair "CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE"))
          (#.(lispify-k-pair "CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE"))
          (#.(lispify-k-pair "CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE")))

#+opencl-2.0
(bitfield (#.(lispify "device_svm_capabilities") :base-type #.(lispify "__device_svm_capabilities"))
          (#.(lispify-k-pair "CL_DEVICE_SVM_COARSE_GRAIN_BUFFER"))
          (#.(lispify-k-pair "CL_DEVICE_SVM_FINE_GRAIN_BUFFER"))
          (#.(lispify-k-pair "CL_DEVICE_SVM_FINE_GRAIN_SYSTEM"))
          (#.(lispify-k-pair "CL_DEVICE_SVM_ATOMICS")))

(constantenum (#.(lispify "command_queue_info") :base-type #.(lispify "__command_queue_info"))
              (#.(lispify-k-pair "CL_QUEUE_CONTEXT"))
              (#.(lispify-k-pair "CL_QUEUE_DEVICE"))
              (#.(lispify-k-pair "CL_QUEUE_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_QUEUE_PROPERTIES"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_QUEUE_SIZE")))

(bitfield (#.(lispify "mem_flags") :base-type #.(lispify "__mem_flags"))
          (#.(lispify-k-pair "CL_MEM_READ_WRITE"))
          (#.(lispify-k-pair "CL_MEM_WRITE_ONLY"))
          (#.(lispify-k-pair "CL_MEM_READ_ONLY"))
          (#.(lispify-k-pair "CL_MEM_USE_HOST_PTR"))
          (#.(lispify-k-pair "CL_MEM_ALLOC_HOST_PTR"))
          (#.(lispify-k-pair "CL_MEM_COPY_HOST_PTR"))
          ;; reserved                                         (1 << 6)   
          (#.(lispify-k-pair "CL_MEM_HOST_WRITE_ONLY"))
          (#.(lispify-k-pair "CL_MEM_HOST_READ_ONLY"))
          (#.(lispify-k-pair "CL_MEM_HOST_NO_ACCESS"))
          (#.(lispify-k-pair "CL_MEM_SVM_FINE_GRAIN_BUFFER"))
          (#.(lispify-k-pair "CL_MEM_SVM_ATOMICS"))
          (#.(lispify-k-pair "CL_MEM_KERNEL_READ_AND_WRITE")))

;; FIXME : duplicated --- cl_mem_flags
#+opencl-2.0
(bitfield (#.(lispify "svm_mem_flags") :base-type #.(lispify "__svm_mem_flags"))
          (#.(lispify-k-pair "CL_MEM_READ_WRITE"))
          (#.(lispify-k-pair "CL_MEM_WRITE_ONLY"))
          (#.(lispify-k-pair "CL_MEM_READ_ONLY"))
          (#.(lispify-k-pair "CL_MEM_USE_HOST_PTR"))
          (#.(lispify-k-pair "CL_MEM_ALLOC_HOST_PTR"))
          (#.(lispify-k-pair "CL_MEM_COPY_HOST_PTR"))
          ;; reserved                                         (1 << 6)   
          (#.(lispify-k-pair "CL_MEM_HOST_WRITE_ONLY"))
          (#.(lispify-k-pair "CL_MEM_HOST_READ_ONLY"))
          (#.(lispify-k-pair "CL_MEM_HOST_NO_ACCESS"))
          (#.(lispify-k-pair "CL_MEM_SVM_FINE_GRAIN_BUFFER"))
          (#.(lispify-k-pair "CL_MEM_SVM_ATOMICS"))
          (#.(lispify-k-pair "CL_MEM_KERNEL_READ_AND_WRITE")))

(bitfield (#.(lispify "mem_migration_flags") :base-type #.(lispify "__mem_migration_flags"))
          (#.(lispify-k-pair "CL_MIGRATE_MEM_OBJECT_HOST"))
          (#.(lispify-k-pair "CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED")))

(constantenum (#.(lispify "channel_order") :base-type #.(lispify "__channel_order"))
              (#.(lispify-k-pair "CL_R"))
              (#.(lispify-k-pair "CL_A"))
              (#.(lispify-k-pair "CL_RG"))
              (#.(lispify-k-pair "CL_RA"))
              (#.(lispify-k-pair "CL_RGB"))
              (#.(lispify-k-pair "CL_RGBA"))
              (#.(lispify-k-pair "CL_BGRA"))
              (#.(lispify-k-pair "CL_ARGB"))
              (#.(lispify-k-pair "CL_INTENSITY"))
              (#.(lispify-k-pair "CL_LUMINANCE"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_Rx"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_RGx"))
              #+opencl-1.1
              (#.(lispify-k-pair "CL_RGBx"))
              (#.(lispify-k-pair "CL_DEPTH"))
              (#.(lispify-k-pair "CL_DEPTH_STENCIL"))
              (#.(lispify-k-pair "CL_sRGB"))
              (#.(lispify-k-pair "CL_sRGBx"))
              (#.(lispify-k-pair "CL_sRGBA"))
              (#.(lispify-k-pair "CL_sBGRA"))
              (#.(lispify-k-pair "CL_ABGR")))

(constantenum (#.(lispify "channel_type") :base-type #.(lispify "__channel_type"))
              (#.(lispify-k-pair "CL_SNORM_INT8"))
              (#.(lispify-k-pair "CL_SNORM_INT16"))
              (#.(lispify-k-pair "CL_UNORM_INT8"))
              (#.(lispify-k-pair "CL_UNORM_INT16"))
              (#.(lispify-k-pair "CL_UNORM_SHORT_565"))
              (#.(lispify-k-pair "CL_UNORM_SHORT_555"))
              (#.(lispify-k-pair "CL_UNORM_INT_101010"))
              (#.(lispify-k-pair "CL_SIGNED_INT8"))
              (#.(lispify-k-pair "CL_SIGNED_INT16"))
              (#.(lispify-k-pair "CL_SIGNED_INT32"))
              (#.(lispify-k-pair "CL_UNSIGNED_INT8"))
              (#.(lispify-k-pair "CL_UNSIGNED_INT16"))
              (#.(lispify-k-pair "CL_UNSIGNED_INT32"))
              (#.(lispify-k-pair "CL_HALF_FLOAT"))
              (#.(lispify-k-pair "CL_FLOAT"))
              (#.(lispify-k-pair "CL_UNORM_INT24")))

(constantenum (#.(lispify "mem_object_type") :base-type #.(lispify "__mem_object_type"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_BUFFER"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_IMAGE2D"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_IMAGE3D"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_IMAGE2D_ARRAY"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_IMAGE1D"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_IMAGE1D_ARRAY"))
              (#.(lispify-k-pair "CL_MEM_OBJECT_IMAGE1D_BUFFER"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_MEM_OBJECT_PIPE")))

(constantenum (#.(lispify "mem_info") :base-type #.(lispify "__mem_info"))
              (#.(lispify-k-pair "CL_MEM_TYPE"))
              (#.(lispify-k-pair "CL_MEM_FLAGS"))
              (#.(lispify-k-pair "CL_MEM_SIZE"))
              (#.(lispify-k-pair "CL_MEM_HOST_PTR"))
              (#.(lispify-k-pair "CL_MEM_MAP_COUNT"))
              (#.(lispify-k-pair "CL_MEM_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_MEM_CONTEXT"))
              (#.(lispify-k-pair "CL_MEM_ASSOCIATED_MEMOBJECT"))
              (#.(lispify-k-pair "CL_MEM_OFFSET"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_MEM_USES_SVM_POINTER")))

(constantenum (#.(lispify "image_info") :base-type #.(lispify "__image_info"))
              (#.(lispify-k-pair "CL_IMAGE_FORMAT"))
              (#.(lispify-k-pair "CL_IMAGE_ELEMENT_SIZE"))
              (#.(lispify-k-pair "CL_IMAGE_ROW_PITCH"))
              (#.(lispify-k-pair "CL_IMAGE_SLICE_PITCH"))
              (#.(lispify-k-pair "CL_IMAGE_WIDTH"))
              (#.(lispify-k-pair "CL_IMAGE_HEIGHT"))
              (#.(lispify-k-pair "CL_IMAGE_DEPTH"))
              (#.(lispify-k-pair "CL_IMAGE_ARRAY_SIZE"))
              ;; #-opencl-2.0
              (#.(lispify-k-pair "CL_IMAGE_BUFFER"))
              (#.(lispify-k-pair "CL_IMAGE_NUM_MIP_LEVELS"))
              (#.(lispify-k-pair "CL_IMAGE_NUM_SAMPLES")))

#+opencl-2.0
(constantenum (#.(lispify "pipe_info") :base-type #.(lispify "__pipe_info"))
              (#.(lispify-k-pair "CL_PIPE_PACKET_SIZE"))
              (#.(lispify-k-pair "CL_PIPE_MAX_PACKETS")))

#+opencl-2.0
(constantenum (#.(lispify "pipe_properties") :base-type #.(lispify "__pipe_properties")))


(constantenum (#.(lispify "addressing_mode") :base-type #.(lispify "__addressing_mode"))
              (#.(lispify-k-pair "CL_ADDRESS_NONE"))
              (#.(lispify-k-pair "CL_ADDRESS_CLAMP_TO_EDGE"))
              (#.(lispify-k-pair "CL_ADDRESS_CLAMP"))
              (#.(lispify-k-pair "CL_ADDRESS_REPEAT"))
              (#.(lispify-k-pair "CL_ADDRESS_MIRRORED_REPEAT")))

(constantenum (#.(lispify "filter_mode") :base-type #.(lispify "__filter_mode"))
              (#.(lispify-k-pair "CL_FILTER_NEAREST"))
              (#.(lispify-k-pair "CL_FILTER_LINEAR")))

(constantenum (#.(lispify "sampler_info") :base-type #.(lispify "__sampler_info"))
              (#.(lispify-k-pair "CL_SAMPLER_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_SAMPLER_CONTEXT"))
              (#.(lispify-k-pair "CL_SAMPLER_NORMALIZED_COORDS"))
              (#.(lispify-k-pair "CL_SAMPLER_ADDRESSING_MODE"))
              (#.(lispify-k-pair "CL_SAMPLER_FILTER_MODE"))
              ;; extensions, not included
              ;; (#.(lispify-k-pair "CL_SAMPLER_MIP_FILTER_MODE"))
              ;; (#.(lispify-k-pair "CL_SAMPLER_LOD_MIN"))
              ;; (#.(lispify-k-pair "CL_SAMPLER_LOD_MAX"))
              )

#+opencl-2.0
(constantenum (#.(lispify "sampler_properties") :base-type #.(lispify "__sampler_properties"))
              (#.(lispify-k-pair "CL_SAMPLER_NORMALIZED_COORDS"))
              (#.(lispify-k-pair "CL_SAMPLER_ADDRESSING_MODE"))
              (#.(lispify-k-pair "CL_SAMPLER_FILTER_MODE")))

(bitfield (#.(lispify "map_flags") :base-type #.(lispify "__map_flags"))
          (#.(lispify-k-pair "CL_MAP_READ"))
          (#.(lispify-k-pair "CL_MAP_WRITE"))
          ;; #+opencl-1.2
          (#.(lispify-k-pair "CL_MAP_WRITE_INVALIDATE_REGION")))

(constantenum (#.(lispify "program_info") :base-type #.(lispify "__program_info"))
              (#.(lispify-k-pair "CL_PROGRAM_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_PROGRAM_CONTEXT"))
              (#.(lispify-k-pair "CL_PROGRAM_NUM_DEVICES"))
              (#.(lispify-k-pair "CL_PROGRAM_DEVICES"))
              (#.(lispify-k-pair "CL_PROGRAM_SOURCE"))
              (#.(lispify-k-pair "CL_PROGRAM_BINARY_SIZES"))
              (#.(lispify-k-pair "CL_PROGRAM_BINARIES"))
              (#.(lispify-k-pair "CL_PROGRAM_NUM_KERNELS"))
              (#.(lispify-k-pair "CL_PROGRAM_KERNEL_NAMES")))

(constantenum (#.(lispify "program_build_info") :base-type #.(lispify "__program_build_info"))
              (#.(lispify-k-pair "CL_PROGRAM_BUILD_STATUS"))
              (#.(lispify-k-pair "CL_PROGRAM_BUILD_OPTIONS"))
              (#.(lispify-k-pair "CL_PROGRAM_BUILD_LOG"))
              (#.(lispify-k-pair "CL_PROGRAM_BINARY_TYPE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE")))

(constantenum (#.(lispify "program_binary_type") :base-type #.(lispify "__program_binary_type"))
              (#.(lispify-k-pair "CL_PROGRAM_BINARY_TYPE_NONE"))
              (#.(lispify-k-pair "CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT"))
              (#.(lispify-k-pair "CL_PROGRAM_BINARY_TYPE_LIBRARY"))
              (#.(lispify-k-pair "CL_PROGRAM_BINARY_TYPE_EXECUTABLE")))

(constantenum (#.(lispify "build_status") :base-type #.(lispify "__build_status"))
              (#.(lispify-k-pair "CL_BUILD_SUCCESS"))
              (#.(lispify-k-pair "CL_BUILD_NONE"))
              (#.(lispify-k-pair "CL_BUILD_ERROR"))
              (#.(lispify-k-pair "CL_BUILD_IN_PROGRESS")))

(constantenum (#.(lispify "kernel_info") :base-type #.(lispify "__kernel_info"))
              (#.(lispify-k-pair "CL_KERNEL_FUNCTION_NAME"))
              (#.(lispify-k-pair "CL_KERNEL_NUM_ARGS"))
              (#.(lispify-k-pair "CL_KERNEL_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_KERNEL_CONTEXT"))
              (#.(lispify-k-pair "CL_KERNEL_PROGRAM"))
              (#.(lispify-k-pair "CL_KERNEL_ATTRIBUTES")))

(constantenum (#.(lispify "kernel_arg_info") :base-type #.(lispify "__kernel_arg_info"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ADDRESS_QUALIFIER"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ACCESS_QUALIFIER"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_TYPE_NAME"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_TYPE_QUALIFIER"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_NAME")))

(constantenum (#.(lispify "kernel_arg_address_qualifier") :base-type #.(lispify "__kernel_arg_address_qualifier"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ADDRESS_GLOBAL"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ADDRESS_LOCAL"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ADDRESS_CONSTANT"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ADDRESS_PRIVATE")))

(constantenum (#.(lispify "kernel_arg_access_qualifier") :base-type #.(lispify "__kernel_arg_access_qualifier"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ACCESS_READ_ONLY"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ACCESS_WRITE_ONLY"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ACCESS_READ_WRITE"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_ACCESS_NONE")))

(constantenum (#.(lispify "kernel_arg_type_qualifier") :base-type #.(lispify "__kernel_arg_type_qualifier"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_TYPE_NONE"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_TYPE_CONST"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_TYPE_RESTRICT"))
              (#.(lispify-k-pair "CL_KERNEL_ARG_TYPE_VOLATILE"))
              #+opencl-2.0
              (#.(lispify-k-pair "CL_KERNEL_ARG_TYPE_PIPE")))

(constantenum (#.(lispify "kernel_work_group_info") :base-type #.(lispify "__kernel_work_group_info"))
              (#.(lispify-k-pair "CL_KERNEL_WORK_GROUP_SIZE"))
              (#.(lispify-k-pair "CL_KERNEL_COMPILE_WORK_GROUP_SIZE"))
              (#.(lispify-k-pair "CL_KERNEL_LOCAL_MEM_SIZE"))
              (#.(lispify-k-pair "CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE"))
              (#.(lispify-k-pair "CL_KERNEL_PRIVATE_MEM_SIZE"))
              (#.(lispify-k-pair "CL_KERNEL_GLOBAL_WORK_SIZE"))
              #+opencl-2.1
              (#.(lispify-k-pair "CL_KERNEL_MAX_NUM_SUB_GROUPS"))
              #+opencl-2.1
              (#.(lispify-k-pair "CL_KERNEL_COMPILE_NUM_SUB_GROUPS")))

#+opencl-2.1
(constantenum (#.(lispify "kernel_sub_group_info") :base-type #.(lispify "__kernel_sub_group_info"))
              (#.(lispify-k-pair "CL_KERNEL_MAX_SUB_GROUP_SIZE_FOR_NDRANGE"))
              (#.(lispify-k-pair "CL_KERNEL_SUB_GROUP_COUNT_FOR_NDRANGE"))
              (#.(lispify-k-pair "CL_GET_LOCAL_SIZE_FOR_SUB_GROUP_COUNT")))

#+opencl-2.0
(constantenum (#.(lispify "kernel_exec_info") :base-type #.(lispify "__kernel_exec_info"))
              (#.(lispify-k-pair "CL_KERNEL_EXEC_INFO_SVM_PTRS"))
              (#.(lispify-k-pair "CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM")))

(constantenum (#.(lispify "event_info") :base-type #.(lispify "__event_info")) 
              (#.(lispify-k-pair "CL_EVENT_COMMAND_QUEUE"))
              (#.(lispify-k-pair "CL_EVENT_COMMAND_TYPE"))
              (#.(lispify-k-pair "CL_EVENT_REFERENCE_COUNT"))
              (#.(lispify-k-pair "CL_EVENT_COMMAND_EXECUTION_STATUS"))
              (#.(lispify-k-pair "CL_EVENT_CONTEXT")))

(constantenum (#.(lispify "command_type") :base-type #.(lispify "__command_type"))
              (#.(lispify-k-pair "CL_COMMAND_NDRANGE_KERNEL"))
              (#.(lispify-k-pair "CL_COMMAND_TASK"))
              (#.(lispify-k-pair "CL_COMMAND_NATIVE_KERNEL"))
              (#.(lispify-k-pair "CL_COMMAND_READ_BUFFER"))
              (#.(lispify-k-pair "CL_COMMAND_WRITE_BUFFER"))
              (#.(lispify-k-pair "CL_COMMAND_COPY_BUFFER"))
              (#.(lispify-k-pair "CL_COMMAND_READ_IMAGE"))
              (#.(lispify-k-pair "CL_COMMAND_WRITE_IMAGE"))
              (#.(lispify-k-pair "CL_COMMAND_COPY_IMAGE"))
              (#.(lispify-k-pair "CL_COMMAND_COPY_IMAGE_TO_BUFFER"))
              (#.(lispify-k-pair "CL_COMMAND_COPY_BUFFER_TO_IMAGE"))
              (#.(lispify-k-pair "CL_COMMAND_MAP_BUFFER"))
              (#.(lispify-k-pair "CL_COMMAND_MAP_IMAGE"))
              (#.(lispify-k-pair "CL_COMMAND_UNMAP_MEM_OBJECT"))
              (#.(lispify-k-pair "CL_COMMAND_MARKER"))
              (#.(lispify-k-pair "CL_COMMAND_ACQUIRE_GL_OBJECTS"))
              (#.(lispify-k-pair "CL_COMMAND_RELEASE_GL_OBJECTS"))
              (#.(lispify-k-pair "CL_COMMAND_READ_BUFFER_RECT"))
              (#.(lispify-k-pair "CL_COMMAND_WRITE_BUFFER_RECT"))
              (#.(lispify-k-pair "CL_COMMAND_COPY_BUFFER_RECT"))
              (#.(lispify-k-pair "CL_COMMAND_USER"))
              (#.(lispify-k-pair "CL_COMMAND_BARRIER"))
              (#.(lispify-k-pair "CL_COMMAND_MIGRATE_MEM_OBJECTS"))
              (#.(lispify-k-pair "CL_COMMAND_FILL_BUFFER"))
              (#.(lispify-k-pair "CL_COMMAND_FILL_IMAGE"))
              (#.(lispify-k-pair "CL_COMMAND_SVM_FREE"))
              (#.(lispify-k-pair "CL_COMMAND_SVM_MEMCPY"))
              (#.(lispify-k-pair "CL_COMMAND_SVM_MEMFILL"))
              (#.(lispify-k-pair "CL_COMMAND_SVM_MAP"))
              (#.(lispify-k-pair "CL_COMMAND_SVM_UNMAP")))

(constantenum (command-execution-status :base-type int)
              (#.(lispify-k-pair "CL_COMPLETE"))
              (#.(lispify-k-pair "CL_RUNNING"))
              (#.(lispify-k-pair "CL_SUBMITTED"))
              (#.(lispify-k-pair "CL_QUEUED")))

(constantenum (#.(lispify "buffer_create_type") :base-type #.(lispify "__buffer_create_type")) 
              (#.(lispify-k-pair "CL_BUFFER_CREATE_TYPE_REGION")))

(constantenum (#.(lispify "profiling_info") :base-type #.(lispify "__profiling_info")) 
              (#.(lispify-k-pair "CL_PROFILING_COMMAND_QUEUED"))
              (#.(lispify-k-pair "CL_PROFILING_COMMAND_SUBMIT"))
              (#.(lispify-k-pair "CL_PROFILING_COMMAND_START"))
              (#.(lispify-k-pair "CL_PROFILING_COMMAND_END"))
              (#.(lispify-k-pair "CL_PROFILING_COMMAND_COMPLETE")))

