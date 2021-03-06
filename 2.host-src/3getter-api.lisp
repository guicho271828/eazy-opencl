#|

Opencl interface is completely imperative, so we need a good interface.
Largely forked from cl-opencl-3b.

This file contain only the definition of the macro define-info-getter.
Actual use of this macro is in functional-definitions.lisp.

|#

(in-package :eazy-opencl.host)

;;; info-getter

(defmacro define-info-getter (name (&rest args) (type) &body body)
  "Define a functional wrapper for get-XXX-info apis.
NAME: The name of the resulting wrapper. MUST be the same name as the underlying api in EAZY-OPENCL.ERROR.
ARGS: The parameters of the resulting wrapper.
TYPE: enum type accepted by FUN as a parameter.
BODY: Query specification of the getter, the most complicated part of the OpenCL API.
      Each element of the body may take either of the 4 forms:

 (:QUERY RESULT_TYPE)

 The simplest case, where RESULT-TYPE
 should be a CFFI type of the value returned by the API.

 (:QUERY :STRING)

 Another simple case. The result is converted to the lisp string.

 (:QUERY RESULT_TYPE :FIXEDSIZE [number])

 The size of the result is [number]*sizeof(result_type).

 (:QUERY RESULT_TYPE :QUERYSIZE [:QUERY2])

 The size of the result is [number]*sizeof(result_type), where [number] is
 obtained by another call to FUN with :QUERY2. For
 example, :MAX-WORK-ITEM-SIZES in GET-DEVICE-INFO requires another call to
 GET-DEVICE-INFO with :MAX-WORK-ITEM-DIMENSIONS, which returns the size.

 (:QUERY NIL :ARRAY t)

 The result is an array of unknown size, and the size can be queried with the api itself by
 passing NULL argument.

 (:QUERY NIL :FORM FORM)
 
 Last resort. In place of the default behavior, FORM is used in the expansion.

"
  (let ((param (lastcar args))
        (fun (find-symbol (symbol-name name) (find-package "%OCL/E"))))
    ;; check if each parameter is valid
    (iter (for form in body)
          (ematch form
            ((list* pname _)
             (assert (foreign-enum-value type pname)))))
    ;; check if there is no missing parameter
    (unless (set-equal (mapcar #'first body)
                       (foreign-enum-keyword-list type))
      (simple-style-warning
       "~<These paramters are not implemented: ~a Contact the author and check ~a~:@>"
       (list (set-difference (foreign-enum-keyword-list type)
                             (mapcar #'first body))
             (asdf:system-relative-pathname
              :eazy-opencl
              "2.host-src/functional-definitions.lisp"))))
    `(defun ,name (,@args)
       (ecase ,param
         ,@(mapcar (curry #'info-getter-case-form name args fun) body)))))

(defun info-getter-case-form (name args fun form)
  (ematch form
    ((list* pname _ (plist :fixedsize (number)))
     `(,pname ,(%fixed-size-case args fun form)))
    ((list* pname _ (plist :querysize (keyword)))
     `(,pname ,(%dynamic-size-case name args fun form)))
    ((list* pname _ (plist :array t))
     `(,pname ,(%array-case args fun form)))
    ((list pname _ :form code)
     `(,pname ,code))
    ((list pname :string)
     `(,pname ,(%string-case args fun pname)))
    ((list pname _)
     `(,pname ,(%simple-case args fun form)))))

(defun %base-type (form)
  (destructuring-bind (pname type &key plist &allow-other-keys) form
    (declare (ignorable pname))
    (if plist
        ;; fixme: any exported way to do this?
        (cffi::canonicalize-foreign-type type)
        type)))

(defun %fixed-size-case (args fun form)
  (let ((base-type (%base-type form)))
    (destructuring-bind (pname type &key fixedsize plist) form
      `(let ((count ,fixedsize))
         (with-foreign-object (value ',base-type count)
           (,fun ,@(butlast args) ,pname
                 (* ,(foreign-type-size type) count)
                 value
                 (cffi:null-pointer))
           (loop for i below count
                 for v = (mem-aref value ',type i)
                 ,@(when plist
                     `(for prop = t then (not prop)
                           when (and prop (not (zerop v)))
                           collect (foreign-enum-keyword ',type v)
                           else))
                 collect v))))))

(defun %dynamic-size-case (name args fun form)
  (let ((base-type (%base-type form)))
    (destructuring-bind (pname type &key querysize plist) form
      `(let ((count
              ;; further call to NAME for obtaining the size
              (,name ,@(butlast args) ,querysize)))
         (with-foreign-object (value ',base-type count)
           (,fun ,@(butlast args) ,pname
                 (* ,(foreign-type-size type) count)
                 value
                 (cffi:null-pointer))
           (loop for i below count
                 for v = (mem-aref value ',type i)
                 ,@(when plist
                     `(for prop = t then (not prop)
                           when (and prop (not (zerop v)))
                           collect (foreign-enum-keyword ',type v)
                           else))
                 collect v))))))

(defun %array-case (args fun form)
  (let ((base-type (%base-type form)))
    (destructuring-bind (pname type &key plist &allow-other-keys) form
      `(with-foreign-object (size '%ocl:size-t)
         (,fun ,@(butlast args) ,pname
               0 (cffi:null-pointer)
               size)
         (let ((count (floor (mem-aref size '%ocl:size-t)
                             ,(foreign-type-size type))))
           (with-foreign-object (value ',base-type count)
             (,fun ,@(butlast args) ,pname
                   (* ,(foreign-type-size type)
                      count)
                   value
                   (cffi:null-pointer))
             (loop for i below count
                   for v = (mem-aref value ',base-type i)
                   ,@ (when plist
                        `(for prop = t then (not prop)
                              when (and prop (not (zerop v)))
                              collect (foreign-enum-keyword ',type v)
                              else))
                   collect v)))))))

(defun %string-case (args fun pname)
  `(with-foreign-object (size '%ocl:size-t)
     (,fun ,@(butlast args) ,pname 0 (cffi:null-pointer) size)
     (let ((count (mem-aref size '%ocl:size-t)))
       ;; char is not imported due to conflict with cl:char
       (with-foreign-object (str '%ocl/g:char count)
         (,fun ,@(butlast args) ,pname count str (cffi:null-pointer))
         ;; for CCL
         (handler-case
             (foreign-string-to-lisp str :encoding :ascii)
           (error (c)
             (warn "~a occured during foreign-string-to-lisp. Returning \"\", so the results is unreliable"
                   (type-of c))
             ""))))))

(defun %simple-case (args fun form)
  (ematch form
    ((list pname type)
     `(with-foreign-object (value ',type)
        (,fun ,@(butlast args) ,pname
              ,(foreign-type-size type) value
              (cffi:null-pointer))
        (mem-aref value ',type)))))



