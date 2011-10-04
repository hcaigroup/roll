(in-package :roll)

;;; class definitions
(defclass learning-system ()
  ( (learning-problem-name :initarg :learning-problem-name :initform "" :accessor learning-problem-name)
    (learning-signature :initarg :learning-signature :initform () :accessor learning-signature)
    (learned-function-file :initarg :learned-function-file :initform nil :accessor learned-function-file)
    (learned-function-package :initarg :learned-function-package :initform nil
                              :accessor learned-function-package) ))

(defclass learning-experience (abstract-experience)
  ( (learning-system :initarg :learning-system :initform nil :accessor learning-system) ))


;;; generic functions
(defgeneric do-learning (learning-system experience))
(defgeneric integrate-learned-function (learning-system learning-problem))


;;; methods

;; integrate-learned-function
(defmethod integrate-learned-function :after ((ls learning-system) (lp learning-problem))
  ; load function file
  (load (learned-function-file ls)))
  ; set status to :learned

;;; macro
(defmacro define-learning-system (name &body args)
  `(progn
    (defclass ,name ,@args)))
