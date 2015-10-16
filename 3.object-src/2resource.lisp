;;; base structure

(in-package :eazy-opencl.object)

(defstruct box
  (value 0 :read-only t))

;; (defstruct (myval (:include box) (:constructor myval (value))))
;; (defvar *mv* (myval 2))
;; (inspect *mv*)
;; (print (myval-value *mv*))

;;; definer

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unboxer (name)
    (symbolicate name '-value))
  (defun creater (base &optional package)
    (let ((name (format nil "CREATE-~a" (symbol-name base)))
          (%package (or (find-package package) *package*)))
      (or (find-symbol name %package)
          (unless package
            (intern name %package))
          (error "~a not found in ~a" name package))))
  (defun releaser (base &optional package)
    (let ((name (format nil "RELEASE-~a" (symbol-name base)))
          (%package (or (find-package package) *package*)))
      (or (find-symbol name %package)
          (unless package
            (intern name %package))
          (error "~a not found in ~a" name package))))
  (defun constructor (name)
    (symbolicate '%make- name)))

(defmacro define-finalized-api (base-name args
                                &key
                                  (creater (creater base-name :eazy-opencl.host))
                                  (releaser (releaser base-name :eazy-opencl.error))
                                  (constructor (constructor base-name) constructor-supplied-p)
                                  sufficient-api)
  "If the api is sufficient (sufficient-api is non-nil),
then base-name is used as the function name.
 Otherwise, a new interned symbol with the same name as e/creater is used."
  (with-gensyms (value box)
    (let* ((defun-name (if sufficient-api base-name (creater base-name)))
           keys rest-p
           (pure-args (iter (with post-rest = 0)
                            (for e in args)
                            (if (< post-rest 2)
                                (match e
                                  ('&optional)
                                  ('&rest
                                   (setf rest-p t)
                                   (incf post-rest))
                                  (_
                                   (when (= 1 post-rest)
                                     (incf post-rest))
                                   (collect e)))
                                (match e
                                  ('&key)
                                  (_ (push e keys)))))))
      `(progn
         ;; simple wrapper
         ,@(unless constructor-supplied-p
             `((defstruct (,base-name (:include box) (:constructor ,constructor (value))))))
         ;; heap version
         (defun ,defun-name ,args
           (declare (ignorable ,@keys))
           (let* ((,value ,(if rest-p
                               `(apply #',creater ,@pure-args)
                               `(,creater ,@pure-args)))
                  (,box (,constructor ,value)))
             (finalize ,box (lambda () (,releaser ,value)))
             ,box))
         ;; stack version --- disabled, since we need one more layer for lisp api
         (defmacro ,(symbolicate 'with- base-name) ((var ,@args) &body body)
           (declare (ignorable ,@keys))
           (let ((%creater-form ,(if rest-p
                                     `(list* ',creater ,@(butlast pure-args) ,@(last pure-args))
                                     `(list ',creater ,@pure-args))))
             `(let ((,var ,%creater-form))
                (unwind-protect
                    (progn
                      ,@body)
                  (,',releaser ,var)))))))))

;;; same creater/releaser name
;;;; context
(define-finalized-api context
    #.`(devices &rest properties
                &key ,@(enum-keywords-as-symbols '%cl:context-properties)))

(define-finalized-api context-from-type
    #.`(type &rest properties
             &key ,@(enum-keywords-as-symbols '%cl:context-properties))
    :releaser %cl/e:release-context
    :constructor %make-context)

;;;; command queue
(define-finalized-api command-queue (context device &optional properties)
  :sufficient-api t)

#+opencl-2.0
(define-finalized-api command-queue-with-properties
    #.`(context device &rest properties &key ,@(enum-keywords-as-symbols '%cl:queue-properties))
    :sufficient-api t
    :releaser %cl/e:release-command-queue
    :constructor %make-command-queue)

;;;; sampler
(define-finalized-api sampler (context normalized-coords addressing-mode filter-mode)
  :sufficient-api t)

#+opencl-2.0
(define-finalized-api sampler-with-properties
    #.`(context &rest properties
                &key ,@(enum-keywords-as-symbols '%cl:sampler-properties))
  :sufficient-api t
  :releaser %cl/e:release-sampler
  :constructor %make-sampler)

;;;; program

(defstruct (program (:include box)
                    (:constructor %make-program (value))))

(define-finalized-api program-with-source (context source)
  :constructor %make-program
  :releaser %cl/e:release-program)

;; (define-finalized-api program-with-binary (context num-devices device-list lengths binaries binary-status)
;;   :releaser %cl/e:release-program)
;; (define-finalized-api program-with-builtin-kernels (context num-devices device-list kernel-names)
;;   :releaser %cl/e:release-program)

;;;; kernel
(define-finalized-api kernel (program kernel-name)
  :sufficient-api t)

(defun create-kernels-in-program (program)
  (declare (ignorable))
  (let* ((vector (eazy-opencl.host:create-kernels-in-program program)))
    (loop for kernel across vector
          do
       (finalize vector
                 (lambda ()
                   (eazy-opencl.error:release-kernel kernel))))
    vector))

(defmacro with-kernels-in-program ((var program) &body body)
  `(let ((,var (eazy-opencl.host:create-kernels-in-program ,program)))
     (unwind-protect (progn ,@body)
       (map nil #'eazy-opencl.error:release-kernel ,var))))




;; (define-finalized-api user-event (context)
;;   :releaser %cl/e:release-event)
;; 
;; 
;; ;;; mem objects
;; 
;; (define-finalized-api buffer (context flags size host-ptr)
;;   :releaser %cl/e:release-mem-object)
;; 
;; #+opencl-1.1
;; (define-finalized-api sub-buffer (buffer flags buffer-create-type buffer-create-info)
;;   :releaser %cl/e:release-mem-object
;;   :sufficient-api t)
;; 
;; #+opencl-1.2
;; (define-finalized-api image (context flags image-format image-desc host-ptr)
;;   :releaser %cl/e:release-mem-object)
;; 
;; ;; #-opencl-1.2
;; (define-finalized-api image-2d (context flags image-format image-width image-height image-row-pitch host-ptr)
;;   :releaser %cl/e:release-mem-object)
;; 
;; ;; #-opencl-1.2
;; (define-finalized-api image-3d (context flags image-format image-width image-height image-depth image-row-pitch image-slice-pitch host-ptr)
;;   :releaser %cl/e:release-mem-object)
;; 
;; #+opencl-2.0
;; (define-finalized-api pipe (context flags pipe-packet-size pipe-max-packets properties)
;;   :releaser %cl/e:release-mem-object)


