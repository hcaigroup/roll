(in-package :roll)

(defmethod do-learning ((ls generic-learning-system) (experience generic-learning-experience))
  (setf (learned-function-code ls)
        (funcall (learning-function ls) (begin-experience-list experience) (end-experience-list experience))))
