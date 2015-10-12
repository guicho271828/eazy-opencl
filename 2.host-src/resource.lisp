;;;; resource management

(in-package :eazy-opencl.host)

;;; utility macros

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun creater (name)
    (intern (format nil "CREATE-~a" (symbol-name name))))

  (defun e/creater (name)
    (or (find-symbol (format nil "CREATE-~a" (symbol-name name))
                     (find-package :EAZY-OPENCL.ERROR))
        (error "no creater of ~a found!" name)))

  (defun releaser (name)
    (or (find-symbol (format nil "RELEASE-~a" (symbol-name name))
                     (find-package :EAZY-OPENCL.ERROR))
        (error "no releaser of ~a found!" name))))

(defmacro define-finalized-api (base-name args
                                &key
                                  (creater (creater base-name))
                                  (e/creater (e/creater base-name))
                                  (releaser (releaser base-name))
                                  sufficient-api)
  "If the api is sufficient (sufficient-api is non-nil),
then base-name is used as the function name.
 Otherwise, a new interned symbol with the same name as e/creater is used."
  (with-gensyms (obj)
    (let ((e/creater-call `(,e/creater ,@args))
          (releaser-call `(,releaser ,obj))
          (defun-name (if sufficient-api base-name creater)))
      `(progn
         ;; heap version
         (defun ,defun-name ,args
           (finalized-let ((,obj ,e/creater-call ,releaser-call))
             ,obj))
         ;; stack version --- disabled, since we need one more layer for lisp api
         #+nil
         (defmacro ,(symbolicate 'with- base-name) ((var ,@args) &body body)
           `(let ((,var (,',e/creater ,,@args)))
              (unwind-protect
                  (progn
                    ,@body)
                (,',releaser ,var))))))))

;;; same creater/releaser name
(define-finalized-api command-queue (context device properties))
(define-finalized-api context (properties num-devices devices pfn-notify user-data))
(define-finalized-api kernel (program kernel-name)
  :sufficient-api t)
(define-finalized-api sampler (context normalized-coords addressing-mode filter-mode))

;;; variations
(define-finalized-api command-queue-with-properties (context device properties)
  :releaser %cl/e:release-command-queue)

#+opencl-2.0
(define-finalized-api sampler-with-properties (context sampler-properties)
  :releaser %cl/e:release-sampler)

(define-finalized-api context-from-type (properties device-type pfn-notify user-data)
  :releaser %cl/e:release-context)
(define-finalized-api program-with-binary (context num-devices device-list lengths binaries binary-status)
  :releaser %cl/e:release-program)
(define-finalized-api program-with-builtin-kernels (context num-devices device-list kernel-names)
  :releaser %cl/e:release-program)
(define-finalized-api program-with-source (context count strings lengths)
  :releaser %cl/e:release-program)
(define-finalized-api user-event (context)
  :releaser %cl/e:release-event)

;;; TODO: array of kernels
#+nil
(define-finalized-api kernels-in-program (program num-kernels kernels num-kernels-ret)
  :releaser %cl/e:release-kernel)

;;; mem objects

(define-finalized-api buffer (context flags size host-ptr)
  :releaser %cl/e:release-mem-object)

#+opencl-1.1
(define-finalized-api sub-buffer (buffer flags buffer-create-type buffer-create-info)
  :releaser %cl/e:release-mem-object
  :sufficient-api t)

#+opencl-1.2
(define-finalized-api image (context flags image-format image-desc host-ptr)
  :releaser %cl/e:release-mem-object)

;; #-opencl-1.2
(define-finalized-api image-2d (context flags image-format image-width image-height image-row-pitch host-ptr)
  :releaser %cl/e:release-mem-object)

;; #-opencl-1.2
(define-finalized-api image-3d (context flags image-format image-width image-height image-depth image-row-pitch image-slice-pitch host-ptr)
  :releaser %cl/e:release-mem-object)

#+opencl-2.0
(define-finalized-api pipe (context flags pipe-packet-size pipe-max-packets properties)
  :releaser %cl/e:release-mem-object)


