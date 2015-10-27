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

(defmacro pie (form)
  (with-gensyms (result)
    `(handler-case
         (let ((,result ,form))
           (format t "~%~<OpenCL Success: ~;~s for ~a (args: ~{~s ~})~:@>"
                   (list ,result ',form (list ,@(cdr form))))
           ,result)
       (%ocl:opencl-error (c)
         (format t "~%~<OpenCL Error:   ~;~s for ~a (args: ~{~s ~})~:@>"
                 (list (%ocl:opencl-error-code c) ',form (list ,@(cdr form))))))))

(defmacro is-string (form &rest reason-args)
  `(is (typep ,form 'string) ,@reason-args))

(defun all-enums (foreign-typename)
  "just an alias"
  (sort (copy-list (foreign-enum-keyword-list foreign-typename)) #'string<))

(defun all-bitfields (foreign-typename)
  "just an alias"
  (sort (copy-list (foreign-bitfield-symbol-list foreign-typename)) #'string<))

(defun test-all-infos (things params name fn)
  (let ((things (ensure-list things)))
    (iter (for param in params)
          (format t "~%OpenCL Test: querying ~a to ~a ~a" param name things)
          (clear-output *standard-output*)
          ;;(sleep 0.1)
          (finishes
            (handler-case
                (format t "~%OpenCL Info:  ~s for query ~a to ~a ~a"
                        (apply fn (append things (list param))) param name things)
              (%ocl:opencl-error (c)
                (format t "~%OpenCL Error: ~s for query ~a to ~a ~a"
                        (%ocl:opencl-error-code c) param name things)))))))


(test setup
  (is-true (get-platform-ids))
  (iter (for pid in (get-platform-ids))
        (test-all-infos pid (all-enums '%ocl:platform-info) :platform #'get-platform-info)
        (finishes
          (iter (for type in (all-bitfields '%ocl:device-type))
                (format t "~&list of device ids with platform-id ~a and type ~a:" pid (list type))
                (for dids = (pie (get-device-ids pid (list type))))
                (iter (for did in dids)
                      (test-all-infos did (all-enums '%ocl:device-info) :device #'get-device-info)
                      (format t "~& getting a context from device ~A and platform ~A" did pid)
                      (for ctx = (create-context (list did) :context-platform pid)) ; :context-platform pid
                      (test-all-infos ctx (all-enums '%ocl:context-info) :context #'get-context-info)
                      (for cq = (pie (create-command-queue ctx did)))
                      (when cq
                        (test-all-infos cq (all-enums '%ocl:command-queue-info) :command-queue #'get-command-queue-info))
                      #+opencl-2.0
                      (for cq2 = (pie (create-command-queue-with-properties ctx did)))
                      #+opencl-2.0
                      (when cq2
                        (test-all-infos cq2 (all-enums '%ocl:command-queue-info) :command-queue #'get-command-queue-info)))
                (for ctx-type = (pie (create-context-from-type type :context-platform pid)))
                (when ctx-type
                  (test-all-infos ctx-type (all-enums '%ocl:context-info) :context #'get-context-info))))))

(defvar *helloworld* "

#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable

__constant char hw[] = \"Hello, World\";

__kernel void hello(__global char * out) {
   size_t tid = get_global_id(0);
   out[tid] = hw[tid];
}

")

(test helloworld
  ;; http://developer.amd.com/tools-and-sdks/opencl-zone/opencl-resources/introductory-tutorial-to-opencl/
  (iter (for pid in (get-platform-ids))
        (iter (for type in (all-bitfields '%ocl:device-type))
              ;; in this example, we do not care the device id
              (for ctx = (pie (create-context-from-type type :context-platform pid)))
              (unless ctx
                (skip "No context found for the device type ~a in platform ~a" type pid)
                (next-iteration))
              (for devices = (get-context-info ctx :context-devices))
              (is (typep devices 'list))
              (for did = (first devices))
              (unless did
                (skip "No device found for the context ~a" ctx)
                (next-iteration))
              #-opencl-2.0
              (for cq = (pie (create-command-queue ctx did)))
              #+opencl-2.0
              (for cq = (pie (create-command-queue-with-properties ctx did)))
              (unless cq
                (skip "Command queue for ctx ~a and device ~a (~a) was not created" ctx did type)
                (next-iteration))
              (is (string=
                   "Hello, World"
                   (print
                    (with-foreign-pointer-as-string ((out-host size) 13) ;; Hello, World<null> : char[13]
                      (let* ((out-device (create-buffer ctx '(:mem-write-only :mem-use-host-ptr) size out-host))
                             (program (build-program (create-program-with-source ctx *helloworld*) :devices (list did)))
                             (kernel (create-kernel program "hello")))
                        (test-all-infos out-device (all-enums '%ocl:mem-info) :buffer #'get-mem-object-info)
                        (test-all-infos program    (all-enums '%ocl:program-info) :program #'get-program-info)
                        (test-all-infos (list program did) (all-enums '%ocl:program-build-info) :build #'get-program-build-info)
                        (test-all-infos kernel     (all-enums '%ocl:kernel-info) :kernel #'get-kernel-info)
                        (set-kernel-arg kernel 0 out-device '%ocl:mem)
                        ;; run the kernel
                        (%ocl/h::with-foreign-array (global-work-size '%ocl:size-t (list size))
                          (pie (%ocl:enqueue-nd-range-kernel cq kernel 1 (cffi:null-pointer) global-work-size (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
                        (pie
                         (%ocl:enqueue-read-buffer cq out-device %ocl:true 0 size out-host 0 (cffi:null-pointer) (cffi:null-pointer)))))))))))
#+nil
(test helloworld-fancy
  ;; http://developer.amd.com/tools-and-sdks/opencl-zone/opencl-resources/introductory-tutorial-to-opencl/
  (iter (for pid in (get-platform-ids))
        (iter (for type in (all-bitfields '%ocl:device-type))
              (for ctx = (pie (create-context-from-type type :context-platform pid)))
              (unless ctx
                (skip "No context found for the device type ~a in platform ~a" type pid)
                (next-iteration))
              (for devices = (get-context-info ctx :context-devices))
              (is (typep devices 'list))
              (for did = (first devices))
              (unless did
                (skip "No device found for the context ~a" ctx)
                (next-iteration))
              #-opencl-2.0
              (for cq = (pie (create-command-queue ctx did)))
              #+opencl-2.0
              (for cq = (pie (create-command-queue-with-properties ctx did)))
              (unless cq
                (skip "Command queue for ctx ~a and device ~a (~a) was not created" ctx did type)
                (next-iteration))
              (is (string=
                   "Hello, World"
                   (print
                    (with-naive-shared-array (out-host out-device 13)
                      (create-program-with-source ctx *helloworld*)
                      (build-program program :devices (list did))
                      (load-program ctx source)
                      (
                       (let* ((kernel (create-kernel
                                       "hello")))
                         (kernel-call program "hello" out-device
                                      ;; work dim
                                      ;; global work offset
                                      ;; global work size
                                      ;; local work size
                                      )
                         (read-buffer out-device out-host))))))))))
