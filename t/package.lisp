#|
  This file is a part of cl-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :cl-opencl.test
  (:use :cl
        :cl-opencl
        :fiveam
        :iterate :alexandria :trivia))
(in-package :cl-opencl.test)



(def-suite :cl-opencl)
(in-suite :cl-opencl)

;; run test with (run! test-name) 

(test cl-opencl

  )



