
(cl:defpackage #:eazy-opencl.bindings
  (:use :cffi)
  (:import-from :cl :float)
  (:nicknames :%cl)
  (:export
   #:*defined-opencl-functions*
   #:error-code
   #:command-execution-status))

