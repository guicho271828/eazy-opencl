;; groveller file, TBP

;; 5.3 Image objects
(in-package #:eazy-opencl.bindings)

(include "CL/cl.h")

(cstruct image-format
  (:image-channel-order channel-order)
  (:image-channel-data-type channel-type))

(cstruct image_desc
    (:image_type #.(lispify "cl_mem_object_type"))
    (:image_width  size-t)
    (:image_height size_t)
    (:image_depth size_t)
    (:image_array_size size_t)
    (:image_row_pitch size_t)
    (:image_slice_pitch size_t)
    (:num_mip_levels uint)
    (:num_samples uint)
    (#+(or opencl-1.0 opencl-1.1 opencl-1.2)
     :buffer
     #+(or opencl-2.0 opencl-2.1)
     :mem_object
     mem))

