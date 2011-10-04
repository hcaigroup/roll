(in-package :roll)

(defclass libsvm (learning-system)
  ( (model-file :reader model-file :initarg :model-file)
    (parameters :initarg :parameters :initform '(:kernel-type :rbf) :reader parameters) ))

; initialize-instance: set directories, file names and extensions
(defmethod initialize-instance :after ((ls libsvm) &key (model-dir (namestring (user-homedir-pathname)))
                                                        (learned-dir (namestring (user-homedir-pathname))))
  (setf (slot-value ls 'model-file)
        (make-pathname :directory model-dir :name (learning-problem-name ls) :type "model"))
  (setf (slot-value ls 'learned-function-file)
        (make-pathname :directory learned-dir :name (learning-problem-name ls) :type "lisp"))
  (ensure-directories-exist (model-file ls))
  (ensure-directories-exist (learned-function-file ls)))