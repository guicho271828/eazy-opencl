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
  :depends-on (:alexandria :cffi :trivial-garbage)
  :components ((:module "0.bindings-src"
                :components
                ((:file             "1-0-package")
                 (:file             "1-1-library")
                 (:file             "1-2-util-for-grovel")
                 (:cffi-grovel-file "1-3-grovel-version")
                 (:file             "1-4-opencl-version-feature")
                 (:file             "2-0-package")
                 (:cffi-grovel-file "2-1-grovel-cl_platform")
                 (:cffi-grovel-file "2-4-grovel-cl")
                 (:cffi-grovel-file "2-4-grovel-cl-enum")
                 (:cffi-grovel-file "2-5-grovel-buffer")
                 (:cffi-grovel-file "2-6-grovel-image")
                 (:file             "3-0-package")
                 (:file             "3-1-translators")
                 (:file             "3-2-bindings"))
                :serial t))
  :description "Opencl Binding for Common Lisp: Low-level API bindings")
