#|
  This file is a part of eazy-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-opencl.test
  (:use :cl
        :eazy-opencl.host
        :fiveam
        :iterate :alexandria :trivia))
(in-package :eazy-opencl.test)



(def-suite :eazy-opencl)
(in-suite :eazy-opencl)

;; run test with (run! test-name) 

(test platform
      (is-true (get-platform-ids))
      (match (get-platform-ids)
        ((list* id _)
         (is (typep (print (get-platform-info id :platform-profile)) 'string))
         (is (typep (print (get-platform-info id :platform-version)) 'string))
         (is (typep (print (get-platform-info id :platform-name)) 'string))
         (is (typep (print (get-platform-info id :platform-vendor)) 'string))
         (is (typep (print (get-platform-info id :platform-extensions)) 'string)))))





