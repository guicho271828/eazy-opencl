
;; cl_khr_icd
;; not sure if this is actually exposed to clients or not?
(defclextfun ("clIcdGetPlatformIDsKHR" icd-get-platform-ids-khr) int
  (num-entries uint)
  (platforms (:pointer platform-id))
  (num-platforms (:pointer uint)))

