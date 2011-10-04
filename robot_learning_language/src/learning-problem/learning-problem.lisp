(in-package :roll)

;;; class
(defclass learning-problem ()
  ( (learning-system :initarg :learning-system :initform nil :accessor learning-system)
    (experience :initarg :experience :initform nil :accessor experience)
    (input-conversion :initarg :input-conversion :initform nil :accessor input-conversion)
    (output-conversion :initarg :output-conversion :initform nil :accessor output-conversion) )
  (:metaclass learning-problem-mc))


(defmethod initialize-instance :after ((lp learning-problem) &key experience learning-system &allow-other-keys)
  (setf (learning-signature learning-system)
        (list
          (get-experience-signature (experience-automaton experience) :begin)
          (get-experience-signature (experience-automaton experience) :end))))


;;; macro
(create-global-structure :learning-problem)

(defmacro define-learning-problem (&key function
                                        use-experience
                                        input-conversion output-conversion
                                        learning-system)
  (let*( (ls-object-var (gensym "LS-OBJECT"))
         (current-package *package*)
         (learning-problem-class (find-lp-class-for-function-spec function))
         (lp-name (create-lp-name-for-class learning-problem-class function))
         (abstract-experience-to-be-created (listp use-experience))
         (exp-name (if abstract-experience-to-be-created
                     (alexandria:format-symbol current-package "~a-EXPERIENCE" lp-name)
                     use-experience)) )
    `(progn
      ; abstract experience must be defined outside let expression below,
      ; otherwise entry in global structure is not found
      ,@(when abstract-experience-to-be-created
        `((define-abstract-experience ,exp-name ,@use-experience)))
      (let ( (,ls-object-var (make-instance ',(first learning-system)
                                 ,@(rest learning-system)
                                 :learning-problem-name ,lp-name
                                 :learned-function-package (find-package ,(package-name current-package)))) )

        (addgv :learning-problem ,(alexandria:format-symbol :keyword "~a" lp-name)
          (make-instance ',learning-problem-class
            :learning-system ,ls-object-var
            :experience (getgv :experience ',exp-name)
            ,@(apply (symbol-function (alexandria:format-symbol :roll "GET-~a-INITARGS" learning-problem-class))
                     (rest function))
            :input-conversion (expand-input-conversion
                                 ',exp-name ',input-conversion)
            :output-conversion ',output-conversion))
        (when (typep (getgv :experience ',exp-name) 'learning-experience)
          (setf (learning-system (getgv :experience ',exp-name)) ,ls-object-var))))))


;;; generic function
(defgeneric learn (learning-problem))

;;; method
(defmethod learn ((lp learning-problem))
  (update-experience (experience lp))
  (do-learning (learning-system lp) (experience lp))
  (integrate-learned-function (learning-system lp) lp))




