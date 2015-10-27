(defpackage :eazy-opencl.bindings
  (:use :cl :cffi :%ocl/g)
  (:shadow :float :char)
  (:nicknames :%ocl)
  (:export))

