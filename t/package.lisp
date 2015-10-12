#|
  This file is a part of eazy-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-opencl.test
  (:use :cl
        :cffi
        :eazy-opencl.host
        :fiveam
        :iterate :alexandria :trivia))
(in-package :eazy-opencl.test)



(def-suite :eazy-opencl)
(in-suite :eazy-opencl)

;; run test with (run! test-name) 

(defmacro pie (&body form)
  `(print (ignore-errors ,@form)))

(defmacro is-string (form &rest reason-args)
  `(is (typep ,form 'string) ,@reason-args))

(defun all-enums (foreign-typename)
  "just an alias"
  (foreign-enum-keyword-list foreign-typename))

(defun all-bitfields (foreign-typename)
  "just an alias"
  (foreign-bitfield-symbol-list foreign-typename))

(test setup
  (is-true (get-platform-ids))
  (iter (for pid in (get-platform-ids))
        (iter (for param in (all-enums '%cl:platform-info))
              (is-string (get-platform-info pid param)))
        (is-true
         (iter (with result = nil)
               (for type in (all-bitfields '%cl:device-type))
               (for dids = (pie (get-device-ids pid (list type))))
               (when dids (setf result t))
               (iter (for did in dids)
                     (is-true
                      (iter (for param in (all-enums '%cl:device-info))
                            (always (print (get-device-info did param))))))
               (finally (return result)))
         "At least one device type should be accepted!")))

(test context
  )

(test program
  )

(test kernel
  )

(test queue
  )





