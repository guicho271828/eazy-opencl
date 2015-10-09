;; groveller file, TBP

;; 5.3 Image objects
(in-package #:eazy-opencl.bindings)

(include "CL/cl.h")

;; 1.2-5.3.1.1
(cstruct _cl_image_format {
    cl_channel_order        image_channel_order;
    cl_channel_type         image_channel_data_type;
} cl_image_format;

;; 1.2-5.3.1.2
typedef struct _cl_image_desc {
    cl_mem_object_type      image_type;
    size_t                  image_width;
    size_t                  image_height;
    size_t                  image_depth;
    size_t                  image_array_size;
    size_t                  image_row_pitch;
    size_t                  image_slice_pitch;
    cl_uint                 num_mip_levels;
    cl_uint                 num_samples;
#ifdef __GNUC__
    __extension__   /* Prevents warnings about anonymous union in -pedantic builds */
#endif
    union {
      cl_mem                  buffer;
      cl_mem                  mem_object;
    };
} cl_image_desc;



(cffi:defcstruct _image-format
  (image-channel-order channel-order)
  (image-channel-data-type channel-type))

(cffi::defctype image-format _image-format)
