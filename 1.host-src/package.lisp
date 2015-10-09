#|
  This file is a part of eazy-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-opencl.host
  (:use :cl :alexandria :trivia)
  (:import-from :eazy-opencl.bindings
                #:error-code
                #:*defined-opencl-functions*))
