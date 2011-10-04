(in-package :roll)



(defmacro define-raw-experience (name &key specification experience-handling)
  (declare (ignore experience-handling))
  (multiple-value-bind (experience-specs task-specs experience-automaton-def acquisition-code)
                       (traverse-automaton-rec specification nil name)
    (let ( (acquire-experiences-function-name (alexandria:format-symbol *package* "ACQUIRE-EXPERIENCES-~a" name)) )
      `(progn
        (def-plan ,acquire-experiences-function-name ()
          (let*( ,@experience-specs
                 ,@task-specs )
            ,acquisition-code))
        (addgv :experience ',name
          (make-instance 'raw-experience
            :name ',name
            :specification-code ',specification
            :experience-automaton ,experience-automaton-def))))))









