(in-package :eazy-opencl.object)

(defun enum-keywords-as-symbols (foreign-type)
  (mapcar (compose #'intern #'string)
          (foreign-enum-keyword-list foreign-type)))
