(in-package :roll)


(defun add-experience-parent (exp parent)
  (push parent (parent-experiences exp))
  (when (typep 'parent 'transient-experience)
        (push exp (slot-value parent 'internal-child-experiences))))


;;; define-abstract-experience
(defmacro define-abstract-experience (name &key specification
                                                parent-experience
                                                (experience-class 'transient-abstract-experience)
                                                experience-class-initargs)

  (multiple-value-bind (experience-automaton-def conversion-code)
                       (analyze-abstract-experience-definition
                         specification name (get-automaton-type-for-experience-class experience-class))
    `(progn
      ,@(when parent-experience
          `((define-experience-conversion
              :from-experience ,parent-experience
              :to-experience ,name
              :operations ,specification
              :conversion-code ,conversion-code)))
      (if (isgv :experience ',name)
        (add-experience-parent (getgv :experience ',name) (getgv :experience ',parent-experience))
        (addgv :experience ',name
          (make-instance ',experience-class
            :name ',name
            :parent-experiences (list (getgv :experience ',parent-experience))
            :experience-automaton ,experience-automaton-def
            ,@(when experience-class-initargs experience-class-initargs)))))))


(let ( (experience-class-automaton-types (make-hash-table)) )

  (defun get-automaton-type-for-experience-class (expclass)
    (let ( (res (gethash expclass experience-class-automaton-types)) )
      (or res 'experience-automaton)))

  (defun set-automaton-type-for-experience-class (expclass auttype)
    (setf (gethash expclass experience-class-automaton-types) auttype))

)




(defun analyze-abstract-experience-definition (specification to-exp-name experience-automaton-type)
  (set-from-experience-var)
  (if (not (listp specification))
    (error "[EXPERIENCE CONVERSION] incomprehensible conversion specification: ~s~%" specification)
    (case (first specification)
      ( with-binding
        (multiple-value-bind (experience-automaton-def conversion-sub-code)
                             (analyze-abstract-experience-definition
                               (third specification) to-exp-name experience-automaton-type)
          (values
            experience-automaton-def
            `((let*( ,@(mapcar #'generate-data-conversion (second specification)) )
              ,@conversion-sub-code)))) )
      ( with-filter
        (multiple-value-bind (experience-automaton-def conversion-sub-code)
                             (analyze-abstract-experience-definition
                               (third specification) to-exp-name experience-automaton-type)
          (values
            experience-automaton-def
            `((when ,(replace-automaton-calls (second specification))
              ,@conversion-sub-code)))) )
      ( otherwise
          (parse-abstract-experience
            specification `(experience-automaton (getgv :experience ',to-exp-name)) experience-automaton-type) ))))

