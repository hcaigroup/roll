(in-package :roll)

; method-learning-problem
(define-learning-problem-class method-learning-problem (learning-problem)
  ( (generic-fun :initarg :generic-fun :initform nil :accessor generic-fun)
    (specializers :initarg :specializers :initform nil :accessor specializers) )
  :definition-schema (:method generic-fun &rest specializers)
  :name-generation (format nil "~a-METHOD-~{~a~^-~}" generic-fun (mapcar #'second specializers))
  :procedure-generation (declare (ignore generic-fun specializers)))

; general-function-learning-problem
(define-learning-problem-class general-function-learning-problem (learning-problem)
  ( (funname :initarg :funname :initform nil :accessor funname)
    (args :initarg :args :initform nil :accessor args) )
  :definition-schema (:some-function funname &rest args)
  :name-generation (format nil "~a-FUNCTION" funname)
  :procedure-generation (declare (ignore funname args)))

