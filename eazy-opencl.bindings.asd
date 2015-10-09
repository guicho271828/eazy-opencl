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
                ((:file "0-package")
                 (:file "1-library")
                 (:file "2-0-util-for-grovel")
                 (:cffi-grovel-file "2-1-grovel-version")
                 (:file "2-2-opencl-version-feature")
                 (:cffi-grovel-file "2-3-grovel-cl_platform")
                 (:cffi-grovel-file "2-4-grovel-cl")
                 (:cffi-grovel-file "2-5-grovel-buffer")
                 (:cffi-grovel-file "2-6-grovel-image")
                 (:file "3-util")
                 (:file "4-bindings")
                 (:file "4-buffer-bindings")
                 (:file "4-image-bindings"))))
  :description "Opencl Binding for Common Lisp: Low-level API bindings")
