#|
  This file is a part of cl-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Opencl Binding for Common Lisp

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage cl-opencl-asd
  (:use :cl :asdf))
(in-package :cl-opencl-asd)


(defsystem cl-opencl
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate :alexandria :trivia)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "Opencl Binding for Common Lisp"
  :in-order-to ((test-op (load-op :cl-opencl.test))))
