(in-package :roll)

;; do-learning
(defmethod do-learning ((ls libsvm) (experience libsvm-experience))
  (let ( (model (cl-libsvm:train (cl-libsvm:make-problem
                                   (coerce (libsvm-target-data experience) 'vector)
                                   (coerce (libsvm-input-data experience) 'vector))
                                 (apply #'cl-libsvm:make-parameter (parameters ls)))) )
    (cl-libsvm:save-model model (model-file ls))))

