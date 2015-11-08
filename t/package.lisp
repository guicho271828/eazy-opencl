#|
  This file is a part of eazy-opencl project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-opencl.test
  (:use :cl
        :cffi
        :eazy-opencl.host
        :eazy-opencl.fancy
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
           (format t "~%~<OpenCL Success: ~@;~s for ~s (args: ~{~s ~})~:@>"
                   (list ,result ',form (list ,@(cdr form))))
           ,result)
       (%ocl:opencl-error (c)
         (format t "~%~<OpenCL Error:   ~@;~s for ~s (args: ~{~s ~})~:@>"
                 (list (%ocl:opencl-error-code c) ',form (list ,@(cdr form))))
         nil))))

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
          (format t "~%~<OpenCL Test:     ~@;querying ~s to ~s ~s~:@>" (list param name things)) 
          (finishes
            (handler-case
                (format t "~%~<OpenCL Info:     ~@;~s for query ~s to ~s ~s~:@>"
                        (list (apply fn (append things (list param))) param name things))
              (%ocl:opencl-error (c)
                (format t "~%~<OpenCL Error:    ~@;~s for query ~s to ~s ~s~:@>"
                        (list (%ocl:opencl-error-code c) param name things)))
              (babel:character-decoding-error (c)
                (fail "~%~<character-decoding-error: ~@;~s, Octets: ~s, for query ~s to ~s ~s~:@>"
                        (list c (babel:character-decoding-error-octets c) param name things)))
              (error (c)
                (fail "~<Unexpected error:~@; ~a : ~a -- while calling ~a with ~s~:@>"
                      (list (type-of c) c fn (append things (list param)))))))
          (handler-case
              (trivial-garbage:gc)
            (error (c)
              (fail "~<Unexpected error during GC:~@;~a -- after calling ~a with ~s~:@>"
                    (list c fn (append things (list param)))))))))


(test setup
  (is-true (get-platform-ids))
  (iter (for pid in (get-platform-ids))
        (test-all-infos pid (all-enums '%ocl:platform-info) :platform #'get-platform-info)
        (finishes
          (iter (for type in (all-bitfields '%ocl:device-type))
                (format t "~&list of device ids with platform-id ~s and type ~s:" pid (list type))
                (for dids = (pie (get-device-ids pid (list type))))
                (iter (for did in dids)
                      (test-all-infos did (all-enums '%ocl:device-info) :device #'get-device-info)
                      (format t "~& getting a context from device ~s and platform ~s" did pid)
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
              (for ctx =
                   (or (pie (create-context-from-type type :context-platform pid))
                       (progn
                         (skip "No context found for the device type ~s in platform ~s" type pid)
                         (next-iteration))))
              (for devices =
                   (or (pie (get-context-info ctx :context-devices))
                       (progn
                         (skip "No device found for the context ~s" ctx)
                         (next-iteration))))
              (for did = (first devices))
              (for cq =
                   (or (pie #-opencl-2.0
                            (create-command-queue ctx did)
                            #+opencl-2.0
                            (create-command-queue-with-properties ctx did))
                       (progn
                         (skip "Command queue for ctx ~s and device ~s (~s) was not created" ctx did type)
                         (next-iteration))))
              (for result =
                   (with-foreign-pointer-as-string ((out-host size) 13) ;; Hello, World<null> : char[13]
                     (let* ((out-device
                             (or (pie (create-buffer ctx '(:mem-write-only :mem-use-host-ptr) size out-host))
                                 (next-iteration)))
                            (program
                             (or (pie (create-program-with-source ctx *helloworld*))
                                 (next-iteration))))
                       (or (pie (build-program program :devices (list did)))
                           (next-iteration))
                       (let ((kernel (or (pie (create-kernel program "hello"))
                                         (next-iteration))))
                         (test-all-infos out-device (all-enums '%ocl:mem-info) :buffer #'get-mem-object-info)
                         (test-all-infos program    (all-enums '%ocl:program-info) :program #'get-program-info)
                         (test-all-infos (list program did) (all-enums '%ocl:program-build-info) :build #'get-program-build-info)
                         (test-all-infos kernel     (all-enums '%ocl:kernel-info) :kernel #'get-kernel-info)
                         (finishes
                           (set-kernel-arg kernel 0 out-device '%ocl:mem))
                         ;; run the kernel
                         (%ocl/h::with-foreign-array (global-work-size '%ocl:size-t (list size))
                           (pie (%ocl:enqueue-nd-range-kernel cq kernel 1 (cffi:null-pointer) global-work-size (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
                         (pie
                          (%ocl:enqueue-read-buffer cq out-device %ocl:true 0 size out-host 0 (cffi:null-pointer) (cffi:null-pointer)))))))
              (is (string= "Hello, World" (print result))))))

(test helloworld-from-file
  (iter (for pid in (get-platform-ids))
        (iter (for type in (all-bitfields '%ocl:device-type))
              ;; in this example, we do not care the device id
              (for ctx =
                   (or (pie (create-context-from-type type :context-platform pid))
                       (progn
                         (skip "No context found for the device type ~s in platform ~s" type pid)
                         (next-iteration))))
              (for devices =
                   (or (pie (get-context-info ctx :context-devices))
                       (progn
                         (skip "No device found for the context ~s" ctx)
                         (next-iteration))))
              (for did = (first devices))
              (for cq =
                   (or (pie #-opencl-2.0
                            (create-command-queue ctx did)
                            #+opencl-2.0
                            (create-command-queue-with-properties ctx did))
                       (progn
                         (skip "Command queue for ctx ~s and device ~s (~s) was not created" ctx did type)
                         (next-iteration))))
              (for result =
                   (with-foreign-pointer-as-string ((out-host size) 13) ;; Hello, World<null> : char[13]
                     (let* ((out-device
                             (or (pie (create-buffer ctx '(:mem-write-only :mem-use-host-ptr) size out-host))
                                 (next-iteration)))
                            (program
                             (or (pie (create-program-with-source ctx (asdf:system-relative-pathname :eazy-opencl "t/helloworld.cl")))
                                 (next-iteration))))
                       (or (pie (build-program program :devices (list did)))
                           (next-iteration))
                       (let ((kernel (or (pie (create-kernel program "hello"))
                                         (next-iteration))))
                         (finishes
                           (set-kernel-arg kernel 0 out-device '%ocl:mem))
                         ;; run the kernel
                         (%ocl/h::with-foreign-array (global-work-size '%ocl:size-t (list size))
                           (pie (%ocl:enqueue-nd-range-kernel cq kernel 1 (cffi:null-pointer) global-work-size (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
                         (pie
                          (%ocl:enqueue-read-buffer cq out-device %ocl:true 0 size out-host 0 (cffi:null-pointer) (cffi:null-pointer)))))))
              (is (string= "Hello, World" (print result))))))

(test with-easy-opencl-setup
  (with-easy-opencl-setup (platform
                           (device (lambda (device)
                                     (eq (get-device-info device :device-type)
                                         :device-type-gpu)))
                           ctx
                           queue)
    (is (atom device))
    (is (atom device))
    (is (atom ctx))
    (is (atom queue))))

#+nil
(test fancy-memory-interface
  (with-easy-opencl-setup (platform
                           (device (lambda (device)
                                     (eq (get-device-info device :device-type)
                                         :device-type-gpu)))
                           ctx
                           queue)
    (is (string=
         "Hello, World"
         (print
          (with-naive-shared-array (out-host out-device 13)
            (load-program ctx source)
            (kernel-call (load-program ctx source) "hello" out-device
                         ;; work dim
                         ;; global work offset
                         ;; global work size
                         ;; local work size
                         )
            (read-buffer out-device out-host)))))))
