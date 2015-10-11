
(in-package :eazy-opencl.host)

(defmacro get-counted-list (fun (&rest args) type)
  "Handles some apis returning an array."
  (with-gensyms (fcount count buffer i)
    `(with-foreign-object (,fcount '%cl:uint)
       ;; the total length is unknown, so calls it once
       (,fun ,@args 0 (cffi:null-pointer) ,fcount)
       (let ((,count (mem-aref ,fcount '%cl:uint)))
         (when (> ,count 0)
           (with-foreign-object (,buffer ,type (1+ ,count))
             ;; then call the same function again to obtain the real array
             (,fun ,@args (1+ ,count) ,buffer ,fcount)
             (loop for ,i below ,count
                collect (mem-aref ,buffer ,type ,i))))))))

#+prototype
(defun get-platform-ids ()
  "returns a list of available OpenCL Platform IDs"
  ;; FIXME: figure out if returning same pointer twice is correct,
  ;; possibly remove-duplicates on it if so?
  (without-fp-traps
    (get-counted-list %cl/e:get-platform-ids () '%cl:platform-id)))

(defmacro define-list-api (name args function type)
  `(defun ,name ,args
     (without-fp-traps
      (get-counted-list ,function ,args ',type))))

(define-list-api get-platform-ids () %cl/e:get-platform-ids %cl:platform-id)
(define-list-api get-device-ids (platform-id device-types) %cl/e:get-device-ids %cl:device-id)
(define-list-api get-supported-image-formats (context flags image-type) %cl/e:get-supported-image-formats %cl:image-format)

