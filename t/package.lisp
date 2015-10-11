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

(defmacro pie (&body form)
  `(print (ignore-errors ,@form)))

(defmacro is-string (form &rest reason-args)
  `(is (typep ,form 'string) ,@reason-args))

(defun all-enums (foreign-typename)
  (alexandria:hash-table-values
   (cffi::value-keywords
    (cffi::parse-type foreign-typename))))

(test setup
  (is-true (get-platform-ids))
  (iter (for id in (get-platform-ids))
        (iter (for param in (all-enums '%cl:platform-info))
              (is-string (get-platform-info id param)))
        (is-true
         (iter (for type in (all-enums '%cl:device-type))
               (for dids = (pie (get-device-ids pid type)))
               (unless dids (next-iteration))
               (iter (for param in (all-enums '%cl:device-info))
                     (is-true (pie (get-device-info pid param)))))
         "At least one device type should be accepted!")))

(test context
  )

(test program
  )

(test kernel
  )

(test queue
  )





