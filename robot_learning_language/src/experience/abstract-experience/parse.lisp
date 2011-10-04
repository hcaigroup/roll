(in-package :roll)

; moves through the hierarchical automaton definition, doing the following:
; generate code exploiting the available recursive structure of the definition:
;    * code for creating experience-automaton instance (used in macro define-abstract-experience)
;    * code for conversion function
; -> returns 2 values

(let ( from-experience-var )

  (defun set-from-experience-var ()
    (setf from-experience-var (gensym "FROM-EXPERIENCE")))

  (defun get-from-experience-var ()
    from-experience-var)

  (defun generate-data-conversion (conv-spec)
    (let ( (var-name (first conv-spec))
           (processing-spec (rest conv-spec)) )
      (multiple-value-bind (var-conversion-code let-vars) (replace-automaton-calls processing-spec)
        (values
          `(,var-name ,@var-conversion-code)
          let-vars))))

  (defun replace-automaton-calls (spec)
    (cond ( (atom spec)
            spec )
          ( (and (listp spec) (eq (first spec) :var))
            (let ( (requested-var-name (second spec))
                   (event (if (listp (third spec)) (first (third spec)) (third spec)))
                   (autname (when (listp (third spec)) (second (third spec))))
                   (occurrence-spec (fourth spec)) )
              `(get-experience-data
                 ,(if autname
                   `(find-sub-automaton ,from-experience-var ',autname)
                   from-experience-var)
                 ,event
                 :var ',requested-var-name
                 :occurrence-content
                   ,(cond ( (and occurrence-spec (atom occurrence-spec))
                            occurrence-spec )
                          ( occurrence-spec
                            `',occurrence-spec )
                          ( T
                            :only-occurrence )))) )
          ( T
            (mapcar #'replace-automaton-calls spec) )))

)

(defun let-to-conslist-code (pair)
  `(cons ',(first pair) ,(second pair)))

(defun parse-abstract-experience (operations to-experience-spec experience-automaton-type)
  (destructuring-bind (id &key begin end interval children (occurrence-handling :ignore)) operations
    (let ( child-automaton-definitions
           child-conversion-code )
      (mapcar #'(lambda (child)
                    (multiple-value-bind (aut-def convert-code)
                                         (parse-abstract-experience
                                           child `(find-sub-automaton ,to-experience-spec ',(first child))  experience-automaton-type)
                      (push aut-def child-automaton-definitions)
                      (push convert-code child-conversion-code)))
                children)
      (values
        `(make-instance ',experience-automaton-type
          :name ',id
          :signature '(:begin ,(mapcar #'first begin)
                       :end ,(mapcar #'first end)
                       :interval ,(mapcar #'first interval))
          ,@(when child-automaton-definitions `(:children (list ,@child-automaton-definitions))))
        (let ( (event-data-code `(:begin (list ,@(mapcar
                                                   (alexandria:compose #'let-to-conslist-code #'generate-data-conversion)
                                                   begin))
                                  :end (list ,@(mapcar
                                                 (alexandria:compose #'let-to-conslist-code #'generate-data-conversion)
                                                 end))
                                  :interval (list ,@(mapcar
                                                 (alexandria:compose #'let-to-conslist-code #'generate-data-conversion)
                                                 interval)))) )
          (case occurrence-handling
            ( :ignore
              `((add-experience-data ,to-experience-spec
                                      (make-instance 'experience-data ,@event-data-code))
                  ,@(when child-conversion-code
                    (mapcar #'first child-conversion-code))) )
            ( otherwise
              (let ( (loopvar (gensym)) )
              `((dolist (,loopvar (construct-exp-data-occurrences
                                    (signature ,to-experience-spec)
                                    ,@event-data-code
                                    :occurrence-handling ,occurrence-handling))
                (add-experience-data ,to-experience-spec (car ,loopvar) (cdr ,loopvar))))) )))))))

; construct-exp-data-occurrences
; takes data in which occurrences are indicated in single variable slots and constructs a list of experience-data instances with occurrence data
; each list element is a cons composed of the experience-data instance and the occurrence list

(let ( occurrence-data ) ; hash table

  (defun sort-data-into-occurrences (event-data event-data-type occurrence-spec)
    (let ( (occ-event-data (gethash occurrence-spec occurrence-data)) )
      (if occ-event-data
        (push event-data
              (slot-value occ-event-data (intern (symbol-name event-data-type) :roll)))
        (setf (gethash occurrence-spec occurrence-data)
              (funcall #'make-instance 'experience-data event-data-type (list event-data))))))

  (defun sort-event-data-into-occurrences (event-data event-type)
    (mapcar #'(lambda (vardat)
                (let ( (varname (first vardat)) )
                  (mapcar #'(lambda (dat)
                    (sort-data-into-occurrences (cons varname (first dat)) event-type (second dat)))
                  (rest vardat))))
            event-data))

  (defun construct-exp-data-occurrences (signature &key begin end interval occurrence-handling)
    (setf occurrence-data (make-hash-table :test #'equal))
    (sort-event-data-into-occurrences begin :begin)
    (sort-event-data-into-occurrences end :end)
    (sort-event-data-into-occurrences interval :interval)
    (case occurrence-handling
      ( :use-occurrences
        (let ( result )
          (maphash #'(lambda (key val) (push (cons (order-experience-data val signature) key) result))
                   occurrence-data)
          result) )
      ( otherwise
        (error "[ABSTRACT EXPERIENCE] occurrence handling ~s not yet implemented." occurrence-handling) )))

)
