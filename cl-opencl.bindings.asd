#|
  This file is a part of eazy-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Opencl Binding for Common Lisp

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage eazy-opencl-asd
  (:use :cl :asdf))
(in-package :eazy-opencl-asd)


(defsystem eazy-opencl.bindings
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:alexandria :cffi)
  :components ((:module "0.bindings-src"
                :components
                ((:file "package"))))
  :description "Opencl Binding for Common Lisp: Low-level API bindings")