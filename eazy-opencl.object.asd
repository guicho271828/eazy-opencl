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


(defsystem eazy-opencl.object
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
  :components ((:module "3.object-src"
                :components
                ((:file "0package")
                 (:file "1util")
                 (:file "2resource"))
                :serial t))
  :description "Opencl Binding for Common Lisp: User API")
