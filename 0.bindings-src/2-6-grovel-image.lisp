;; groveller file, TBP

;; 5.3 Image objects
(in-package #:eazy-opencl.bindings)

(include "CL/cl.h")

(cstruct image-format "cl_image_format"
  (:channel-order "image_channel_order" :type #.(lispify "channel_order"))
  (:channel-data-type "image_channel_data_type" :type #.(lispify "channel_type")))

(cstruct image-desc "cl_image_desc"
    (#.(lispify-k "image_type") "image_type" :type #.(lispify "mem_object_type"))
    (#.(lispify-k "image_width") "image_width" :type size-t)
    (#.(lispify-k "image_height") "image_height" :type size-t)
    (#.(lispify-k "image_depth") "image_depth" :type size-t)
    (#.(lispify-k "image_array_size") "image_array_size" :type size-t)
    (#.(lispify-k "image_row_pitch") "image_row_pitch" :type size-t)
    (#.(lispify-k "image_slice_pitch") "image_slice_pitch" :type size-t)
    (#.(lispify-k "num_mip_levels") "num_mip_levels" :type uint)
    (#.(lispify-k "num_samples") "num_samples" :type uint)
    (#-(or :opencl-2.0 :opencl-2.1) :buffer
     #-(or :opencl-2.0 :opencl-2.1) "buffer"
     #+(or :opencl-2.0 :opencl-2.1) #.(lispify-k "mem_object")
     #+(or :opencl-2.0 :opencl-2.1) "mem_object"
     :type mem))

