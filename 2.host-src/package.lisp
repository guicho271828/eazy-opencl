#|
This file is a part of eazy-opencl project.
Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)

Design:

* Convert the imperative C apis -> functional apis.

Error handling is already achieved by the previous layer.
What we do now is to convert to/from foreign type,
or to handle the apis which sets the result to the given pointer and
returns an error code.

* Garbage collection policy

We do not use RETAIN interface, and instead use the garbage collector of
the host lisp. If the object is unreachable from the memory space of the
host lisp, then it RELEASE the object once, decrementing the reference
count. (There might be more references from the opencl
implementation itself.) This is realized through trivial-garbage:finalize.

* Class Wrapper?

Context, program, kernel, device, commandqueue can be wrapped in a CLOS
layer. The overhead must be smaller than the large computation done by
opencl.

This does not mean that the wrapper should be very fast.  If this overhead
is a problem, then it also means that your application contains a large
number of very costly GPU-MEMORY communications, required each time the
GPGPU computation is initialized. In such cases, you should instead
consider changing the algorithm or even stop using GPGPU.

|#

(in-package :cl-user)
(defpackage eazy-opencl.host
  (:use :cl :cffi :iterate :alexandria :trivia :eazy-opencl.error :trivial-garbage)
  (:shadowing-import-from :eazy-opencl.error :finish))
(in-package :eazy-opencl.host)

;; 

(defmacro finalized-let (extended-bindings &body body)
  "Ensure the let-bound object is finalized through a finalizer form while
  being reclaimed by GC

extended-bindings: [binding*]
binging: (variable value-form finalizer-form*)
"
  `(let ,(mapcar (lambda-match
                   ((list* (and (symbol) var) val finalizer)
                    `(,var
                      (let ((,var ,val))
                        (finalize ,var (lambda () ,@finalizer))))))
                 extended-bindings)
     ,@body))



#+mock
(finalized-let ((cq (create-command-queue context device properties)
                    (release-command-queue cq)))
  :ababab)

;; (create-command-queue context device properties)
;; (release-command-queue command-queue)

#+prototype
(defun command-queue (context device properties)
  (finalized-let ((cq (create-command-queue context device properties)
                      (release-command-queue cq)))
    cq))

(defmacro define-finalized-api (base-name args
                                &key
                                  (creater (symbolicate 'create- base-name))
                                  (releaser (symbolicate 'release- base-name)))
  (with-gensyms (obj)
    `(defun ,base-name ,args
       (finalized-let ((,obj (,creater ,@args)
                             (,releaser ,obj)))
         ,obj))))

;; same creater/releaser name
(define-finalized-api command-queue (context device properties))
(define-finalized-api context (properties num-devices devices pfn-notify user-data))
(define-finalized-api kernel (program kernel-name))
(define-finalized-api sampler (context normalized-coords addressing-mode filter-mode))

;; variations
(define-finalized-api context-from-type (properties device-type pfn-notify user-data)
  :releaser release-context)
(define-finalized-api program-with-binary (context num-devices device-list lengths binaries binary-status)
  :releaser release-program)
(define-finalized-api program-with-builtin-kernels (context num-devices device-list kernel-names)
  :releaser release-program)
(define-finalized-api program-with-source (context count strings lengths)
  :releaser release-program)
(define-finalized-api user-event (context)
  :releaser release-event)

;; TODO: array of kernels
#+nil
(define-finalized-api kernels-in-program (program num-kernels kernels num-kernels-ret)
  :releaser release-kernel)

;; mem objects

(define-finalized-api buffer (context flags size host-ptr)
  :releaser release-mem-object)

#+opencl-1.1
(define-finalized-api sub-buffer (buffer flags buffer-create-type buffer-create-info)
  :releaser release-mem-object)

#+opencl-1.2
(define-finalized-api image (context flags image-format image-desc host-ptr)
  :releaser release-mem-object)

;; #-opencl-1.2
(define-finalized-api image-2d (context flags image-format image-width image-height image-row-pitch host-ptr)
  :releaser release-mem-object)

;; #-opencl-1.2
(define-finalized-api image-3d (context flags image-format image-width image-height image-depth image-row-pitch image-slice-pitch host-ptr)
  :releaser release-mem-object)

#+opencl-2.0
(define-finalized-api pipe (context flags pipe-packet-size pipe-max-packets properties)
  :releaser release-mem-object)

