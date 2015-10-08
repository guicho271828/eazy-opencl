#|
  This file is a part of cl-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage cl-opencl.test-asd
  (:use :cl :asdf))
(in-package :cl-opencl.test-asd)


(defsystem cl-opencl.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-opencl"
  :license "LLGPL"
  :depends-on (:cl-opencl
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :cl-opencl))"))
))
