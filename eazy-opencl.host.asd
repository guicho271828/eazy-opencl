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


(defsystem eazy-opencl.host
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate
               :alexandria
               :trivia
               :trivial-garbage
               :lisp-namespace
               :eazy-opencl.error)
  :components ((:module "2.host-src"
                :components
                ((:file "package")
                 (:file "util")
                 (:file "resource")
                 (:file "functional")
                 (:file "functional-definitions")
                 (:file "list-apis")
                 (:file "launch-steps"))
                :serial t))
  :description "Opencl Binding for Common Lisp: User API")
