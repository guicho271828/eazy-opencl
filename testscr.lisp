
(in-package :cl-user)

(uiop:quit (if (handler-case
                   (progn
                     (asdf:load-system :cl-opencl.test)
                     (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :cl-opencl))"))
)
                 (serious-condition (c)
                   (describe c)
                   (uiop:quit 2)))
               0 1))


