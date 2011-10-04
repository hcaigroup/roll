(in-package :roll)

;; metaclass
(defclass learning-problem-mc (standard-class)
  ( (definition-schema :initarg :definition-schema :initform nil :accessor definition-schema) ))

(defmethod clos:validate-superclass ((class learning-problem-mc) (super standard-class))
  T)


;; define-learning-problem-class
(defmacro define-learning-problem-class (name superclasses slots
                                          &key definition-schema (name-generation nil ngp)
                                               (procedure-generation nil pgp) initargs
                                          &allow-other-keys)
  (let ( (classname (if (eq (symbol-package name) (find-package :roll))
                      name
                      (import name :roll))) )
                      ;; not sure if import works, previously this worked: (intern (symbol-name name) :roll))) )
    `(progn
      (defun ,(alexandria:format-symbol :roll "MAKE-~a-NAME" name) ,(rest definition-schema)
        ,(if ngp
          `(string-upcase ,name-generation)
          `(apply #'make-learning-problem-name ,@(convert-lambda-list-to-apply (rest definition-schema)))))
      (defun ,(alexandria:format-symbol :roll "MAKE-~a-PROC" name) ,(rest definition-schema)
        ,(if pgp
          procedure-generation
          `(apply #'make-learning-problem-proc ,@(convert-lambda-list-to-apply (rest definition-schema)))))
      (defun ,(alexandria:format-symbol :roll "GET-~a-INITARGS" name) ,(rest definition-schema)
        (list
          ,@(merge-initargs
              (mapcan #'(lambda (slot) (when (member (first slot) (rest definition-schema))
                                          `(,(second (member :initarg slot)) `',,(first slot))))
                      slots)
            initargs)))
      (defclass ,classname ,superclasses ,slots
        (:definition-schema ,@definition-schema)
        (:metaclass learning-problem-mc)))))

(defun merge-initargs (automatic manual)
  (if (null manual)
    automatic
    (let ( (in-aut (member (first manual) automatic)) )
      (cond ( in-aut
              (setf (second in-aut) (second manual))
              (merge-initargs automatic (cddr manual)) )
            ( T
              (merge-initargs (cons (first manual) (cons ``',,(second manual) automatic)) (cddr manual)) ))) ))


(defun convert-lambda-list-to-apply (lambda-list)
  (cond ( (null lambda-list)
          '(()) )
        ( (eq (first lambda-list) '&rest)
          (rest lambda-list) )
        ( T
          (cons (first lambda-list) (convert-lambda-list-to-apply (rest lambda-list))) )))

;; functions for finding learning-problem-class for a given function spec and generation of
;; learning problem name

; find-lp-class-for-function-spec
(defun find-lp-class-for-function-spec (function-spec &optional (lp-class (find-class 'learning-problem)))
  (labels ( (match-specification (original-spec test-spec)
              (cond ( (null original-spec)
                      (null test-spec) )
                    ( (eq (first original-spec) '&rest)
                      T )
                    ( T
                      (match-specification (rest original-spec) (rest test-spec)) ))) )
    (cond ( (and (eq (first function-spec) (first (definition-schema lp-class)))
                 (match-specification (rest (definition-schema lp-class)) (rest function-spec)))
            (class-name lp-class) )
          ( T
            (find-if (alexandria:compose #'not #'null)
              (mapcar #'(lambda (cl) (find-lp-class-for-function-spec function-spec cl))
                      (clos:class-direct-subclasses lp-class))) ))))

; create-lp-name-for-class
(defun create-lp-name-for-class (lp-class-name function-spec)
  (apply (symbol-function (alexandria:format-symbol :roll "MAKE-~a-NAME" lp-class-name))
         (rest function-spec)))

; create-proc-name-for-class
(defun create-proc-name-for-class (lp-class-name function-spec)
  (apply (symbol-function (alexandria:format-symbol :roll "MAKE-~a-PROC" lp-class-name))
         (rest function-spec)))

; create-lp-name-for-function-spec
(defun create-lp-name-for-function-spec (function-spec)
  (create-lp-name-for-class (find-lp-class-for-function-spec function-spec) function-spec))




