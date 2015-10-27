(in-package :eazy-opencl.grovel-utils)

(when (boundp 'opencl-1.0) (push :opencl-1.0 *features*))
(when (boundp 'opencl-1.1) (push :opencl-1.1 *features*))
(when (boundp 'opencl-1.2) (push :opencl-1.2 *features*))
(when (boundp 'opencl-2.0) (push :opencl-2.0 *features*))
(when (boundp 'opencl-2.1) (push :opencl-2.1 *features*))
