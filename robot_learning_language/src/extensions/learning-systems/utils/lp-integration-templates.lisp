(in-package :roll)

(defgeneric make-learned-function-call (lp function-body))

;; The function make-learned-function-call may be called by the method integrate-learned-function
;; of a learning system. It provides a standard way to integrate generated code into the different
;; learning problem classes. This simple method is not suitable for all learning systems, e.g. when
;; foreign functions are involved.


; general-function-learning-problem
(defmethod make-learned-function-call ((lp general-function-learning-problem) function-body)
  `(defun ,(funname lp) ,(args lp)
    (let*,(make-learning-conversion-code (input-conversion lp) ())
      ,function-body)))


; method-learning-problem
(defmethod make-learned-function-call ((lp method-learning-problem) function-body)
  (let ( (lambda-list (mapcar #'(lambda (var)
                                  (let ( (spec (find var (specializers lp) :key #'first)) )
                                    (if spec
                                      spec
                                      var)))
                              (clos:generic-function-lambda-list (symbol-function (generic-fun lp))))) )
    `(defmethod ,(generic-fun lp) ,lambda-list
      (let* ,(make-learning-conversion-code (input-conversion lp) nil)
        ,function-body))))


