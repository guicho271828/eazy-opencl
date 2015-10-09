#|
  This file is a part of eazy-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage eazy-opencl.test-asd
  (:use :cl :asdf))
(in-package :eazy-opencl.test-asd)


(defsystem eazy-opencl.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of eazy-opencl"
  :license "LLGPL"
  :depends-on (:eazy-opencl
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :eazy-opencl))"))
))
