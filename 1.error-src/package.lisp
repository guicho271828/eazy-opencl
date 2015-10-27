#|
  This file is a part of eazy-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-opencl.error
  (:use :cl :alexandria :trivia :cffi)
  (:nicknames :%ocl/e)
  (:import-from :eazy-opencl.bindings
                #:error-code
                #:*defined-opencl-functions*)
  (:export
   #:opencl-error
   #:code
   #:form
   #:opencl-error-code
   #:opencl-error-form))
