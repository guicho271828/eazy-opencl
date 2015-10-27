
(in-package :eazy-opencl.grovel-utils)

;; drawn from swig-lispify

(defun lispify (name &optional flag (package *package*))
  (labels ((helper (lst last rest &aux (c (car lst)))
                (cond
                  ((null lst)
                   rest)
                  ((upper-case-p c)
                   (helper (cdr lst) 'upper
                           (case last
                             ((lower) (list* c #\- rest))
                             (t (cons c rest)))))
                  ((lower-case-p c)
                   (helper (cdr lst) 'lower (cons (char-upcase c) rest)))
                  ((digit-char-p c)
                   (helper (cdr lst) 'digit 
                           (cons c rest)))
                  ((char-equal c #\_)
                   (helper (cdr lst) '_ (cons #\- rest)))
                  (t
                   (error "Invalid character: ~A" c)))))
    (let* ((fix (case flag
                     ((constant enumvalue) "+")
                     (variable "*")
                     (t "")))
              (sym (intern
                    (concatenate
                     'string
                     fix
                     (nreverse (helper (concatenate 'list name) nil nil))
                     fix)
                    package)))
      ;;(export sym package)
      sym)))

(defun lispify-k (name &optional flag)
  (lispify name flag (find-package :keyword)))

(defun lispify-k-pair (name &optional flag)
  (assert (zerop (search "CL_" name :test #'char-equal)))
  `(,(lispify (subseq name 3) flag (find-package :keyword)) ,name))

(defun lispify-wo-prefix (name &optional flag)
  (assert (zerop (search "CL_" name :test #'char-equal)))
  (lispify (subseq name 3) flag))

