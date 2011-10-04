(in-package :roll)

(defclass snns (learning-system)
  ( (pattern-file  :reader pattern-file)
    (network-file  :reader network-file)
    (batchman-file :reader batchman-file)
    (result-file   :reader result-file)
    (c-source-file :reader c-source-file)
    (library-file  :reader library-file)
    (unit-activation-fun :reader unit-activation-fun)
    (unit-output-fun :reader unit-output-fun)
    (net-initialization-fun :initarg :net-initialization-fun :initform nil :reader net-initialization-fun)
    (net-learning-fun :initarg :net-learning-fun :initform nil :reader net-learning-fun)
    (hidden-layers :initarg :hidden-layers :initform () :reader hidden-layers)
    (cycle-bias :initarg :cycle-bias :initform 100 :reader cycle-bias :writer set-cycle-bias) ))


; initialize-instance: set directories, file names and extensions
(defmethod initialize-instance :after ((ls snns) &key (root-dir (namestring (user-homedir-pathname)))
                                                      (pattern-dir root-dir) (network-dir root-dir)
                                                      (batchman-dir root-dir)
                                                      (c-source-dir root-dir) (library-dir root-dir)
                                                      (foreign-function-dir root-dir)
                                                      (data-dir root-dir)
                                                      (unit-activation-fun "Logistic")
                                                      (unit-output-fun "Identity"))
  ; adapt paths
  (let*( (root-path (make-pathname :directory root-dir))
         (data-path (merge-pathnames (make-pathname :directory data-dir) root-path)) )
    (setf (slot-value ls 'pattern-file)
      (merge-pathnames
        (make-pathname :directory pattern-dir :name (learning-problem-name ls) :type "pat")
        data-path))
    (setf (slot-value ls 'network-file)
      (merge-pathnames
        (make-pathname :directory network-dir :name (learning-problem-name ls) :type "net")
        data-path))
    (setf (slot-value ls 'batchman-file)
      (merge-pathnames
        (make-pathname :directory batchman-dir :name (learning-problem-name ls) :type "bat")
        data-path))
    (setf (slot-value ls 'result-file)
      (merge-pathnames
        (make-pathname :directory batchman-dir :name (learning-problem-name ls) :type "res")
        data-path))
    (setf (slot-value ls 'c-source-file)
      (merge-pathnames
        (make-pathname :directory c-source-dir :name (learning-problem-name ls) :type "c")
        root-path))
    (setf (slot-value ls 'library-file)
      (merge-pathnames
        (make-pathname :directory (let ( (arch-dir (if (port:getenv "ARCH_DIR") (port:getenv "ARCH_DIR") "")) )
                                    (if library-dir
                                      (append library-dir (list arch-dir))
                                      (list :relative arch-dir)))
                       :name (learning-problem-name ls) :type "so")
        root-path))
    (unless (learned-function-file ls)
      (setf (learned-function-file ls)
        (merge-pathnames
          (make-pathname :directory foreign-function-dir :name (learning-problem-name ls) :type "lisp")
          root-path))))
  ; set unit functions
  (setf (slot-value ls 'unit-activation-fun)
    (cut:string-concat "Act_" unit-activation-fun))
  (setf (slot-value ls 'unit-output-fun)
    (cut:string-concat "Out_" unit-output-fun)))


