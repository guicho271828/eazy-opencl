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
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:alexandria :cffi)
  :components ((:module "0.bindings-src"
                :components
                ((:file "0.package")
                 (:file "1.library")
                 (:file "2.grovel-tools")
                 (:cffi-grovel-file "grovel-cl_platform")
                 (:cffi-grovel-file "grovel-cl")
                 (:file "3.util")
                 (:file "4.bindings")
                 (:cffi-grovel-file "grovel-buffer")
                 (:file "4.buffer-bindings")
                 (:cffi-grovel-file "grovel-image"))))
  :description "Opencl Binding for Common Lisp: Low-level API bindings")
