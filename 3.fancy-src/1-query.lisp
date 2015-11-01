

(in-package :eazy-opencl.fancy)

(defmacro with-easy-opencl-setup ((platform-query
                                   device-query
                                   context-query
                                   queue-query)
                                  &body body)
  "
*-query : var | (var &optional test)

VAR :

 A symbol naming a variable. Symbol _ (regardless of package) is ignored,
 and can appear multiple times.

TEST :

 A unary boolean test function. When it returns nil, it excludes some
 platform/device etc.  It signals an error when the number of instances
 matched is 0 or more than 2.

BODY : a form to be executed under the binding.
 "
  (destructuring-bind (platform &optional
                                (platform-test '(constantly t))) (ensure-list platform-query)
    (destructuring-bind (device &optional
                                (device-test '(constantly t))) (ensure-list device-query)
      (destructuring-bind (context &optional
                                   (context-test '(constantly t))) (ensure-list context-query)
        (destructuring-bind (queue &optional
                                   (queue-test '(constantly t))) (ensure-list queue-query)
          (let* (ignore-list
                 (variables (iter (for var in (list platform device context queue))
                                  (collect
                                      (ematch var
                                        ((symbol (name "_"))
                                         (with-gensyms (ignored)
                                           (push ignored ignore-list)
                                           ignored))
                                        ((symbol) var))))))
            `(call-with-easy-opencl-setup
              ,platform-test
              ,device-test
              ,context-test
              ,queue-test
              (lambda ,variables
                ,@(when ignore-list
                    `((declare (ignore ,@ignore-list))))
                ,@body))))))))

#+prototype
(with-easy-opencl-setup (platform
                           (device (lambda (device)
                                     (eq (get-device-info device :device-type)
                                         :device-type-gpu)))
                           ctx
                           queue)
    (is (atom device))
    (is (atom device))
    (is (atom ctx))
    (is (atom queue)))

(defun call-with-easy-opencl-setup (platform-test
                                    device-test
                                    context-test
                                    queue-test
                                    callback)
  (let (%pid %device %ctx %queue flag)
    (iter (for pid in (remove-if-not platform-test (get-platform-ids)))
          (for devices = (remove-if-not device-test (get-device-ids pid (list :device-type-all))))
          (unless (< 0 (length devices) 2)
            (next-iteration))
          (for did = (first devices))
          (for ctx = (create-context devices :context-platform pid))
          (unless (funcall context-test ctx)
            (next-iteration))
          (for queue =
               #-opencl-2.0
               (create-command-queue ctx did)
               #+opencl-2.0
               (create-command-queue-with-properties ctx did))
          (unless (funcall queue-test queue)
            (next-iteration))
          (when flag (error "multiple instances found!"))
          (setf %pid pid %device (first devices) %ctx ctx %queue queue flag t))
    (funcall callback %pid %device %ctx %queue)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| unused ideas

 (deftype query-type ()
   `(member :one :many))

QUERY-TYPE : a form which evaluates to a keyword :one, :many or :one-or-many .

 When :one, it signals an error when the number of instances matched is 0 or more than 2.

 When :many, it signals an error when the number of instances matched is 0 or 1.

 When :one-or-many, it signals an error when the number of instances matched is 0.


            (flet ((declare-form (variable query-type type)
                     (match query-type
                       (:one `((declare (type ,type ,variable))))
                       (:many `((declare (type list ,variable)))))))

                  ,@(declare-form platform platform-query-type 'fixnum)
                  ,@(declare-form device device-query-type 'boxed-device-id)
                  ,@(declare-form context context-query-type 'boxed-context)
                  ,@(declare-form queue queue-query-type 'boxed-command-queue)


  (declare (type query-type
                 platform-query-type
                 device-query-type
                 context-query-type
                 queue-query-type))



|#
