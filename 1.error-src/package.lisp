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
                #:finalize-box
                #:*defined-opencl-functions*)
  #.`(:export
      ,@(mapcar (lambda (info)
                  (make-symbol (symbol-name (cadar info))))
                eazy-opencl.bindings:*defined-opencl-functions*)))
