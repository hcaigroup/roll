(in-package :roll)

;;; traverse-automaton-rec
; moves through the hierarchical automaton definition, doing the following:
; 1) collect globally needed information:
;    * experience variable specifications
;    * task specifications
; 2) generate code exploiting the available recursing structure of the definition:
;    * code for creating experience-automaton instance (used in macro define-raw-experience)
;    * code for acquire-experiences function
; -> returns 4 values

; name argument only for first call to create specific experience definition

(defun traverse-automaton-rec (specification &optional parent-experience-aut-var name)
  (destructuring-bind (id &key task invariant begin end interval interval-parameters children) specification
    (destructuring-bind (&key frequency contains-objects) interval-parameters
      (format t "contains objects: ~a~%" contains-objects)
      (when (null id) (setf id (gensym "AUTOMATON-ID")))
      (let ( sub-experience-specs
             sub-task-specs
             child-automaton-definitions
             child-acquisition-codes
             (experience-aut-var (gensym "EXPERIENCE-AUTOMATON"))
             (task-fluent-var (gensym "TASK-FLUENT"))
             (task-var (gensym "TASK"))
             (begin-data-var (gensym "BEGIN-DATA"))
             (end-data-var (gensym "END-DATA"))
             (interval-data-var (gensym "INTERVAL-DATA"))
             (recorder-var (gensym "RECORDER"))
             (interval-signature (mapcar #'first interval))
             (taskspec (when task (create-taskspec task)))
             (request-id (gensym)) )
        (mapcar #'(lambda (child)
                    (multiple-value-bind (experience-spec task-spec aut-def acq-code)
                                         (traverse-automaton-rec child experience-aut-var)
                      (setf sub-experience-specs (append experience-spec sub-experience-specs))
                      (when task-spec
                        (setf sub-task-specs (append task-spec sub-task-specs)))
                      (push aut-def child-automaton-definitions)
                      (push acq-code child-acquisition-codes)))
                children)
        (values
          (cons `(,experience-aut-var
                   ,(if (null parent-experience-aut-var)
                     `(experience-automaton (getgv :experience ',name))
                     `(find-sub-automaton ,parent-experience-aut-var ',id)))
                 sub-experience-specs)
          (if (and task (or (eq (taskspec-type taskspec) :plan)  (eq (taskspec-type taskspec) :tagged)))
            (cons task-fluent-var sub-task-specs)
            sub-task-specs)
          `(make-instance 'experience-automaton
            :name ',id
            :signature '(:begin ,(mapcar #'first begin)
                         :end ,(mapcar #'first end)
                         :interval ,interval-signature)
            ,@(when child-automaton-definitions `(:children(list ,@child-automaton-definitions))))
          (let*( (this-automaton-main-code
                   `(let ( ,@(when task `((,task-var
                                           ,(case (taskspec-type taskspec)
                                             ((:plan :tagged) `(value ,task-fluent-var))))))
                           ,@(when begin `(,begin-data-var))
                           ,@(when end `(,end-data-var))
                           ,@(when interval
                             `(,interval-data-var
                               (,recorder-var
                                 (make-instance 'recorder
                                   :name ',(alexandria:format-symbol :roll "~a-RECORDER" id)
                                   :signature ',interval-signature
                                   ,@(when frequency `(:frequency ,frequency))
                                   :recording-code
                                     ',(if contains-objects
                                       `(list ,@(mapcar #'(lambda (spec)
                                                                 `(copy-object-values ,(second spec)))
                                                        interval))
                                       `(list ,@(mapcar #'second interval))))))) )
                       (wait-for
                         ,(cond ( (and task invariant)
                                  `(fl-and (cpl-impl::task-alive ,task-var)
                                           ,invariant) )
                                ( task
                                  `(cpl-impl::task-alive ,task-var) )
                                ( invariant
                                  invariant )
                                ( T
                                  `(make-fluent :name 'true :value T) )))
                       (unwind-protect
                         (seq
                           ,@(when interval `((activate ,recorder-var)))
                           ,@(when begin
                            `((setf ,begin-data-var
                                    (list ,@(mapcar #'(lambda (bb) (make-data-list-code bb task-var task))
                                                    begin)))))
                           (wait-for
                             ,(cond ( (and task invariant)
                                  `(fl-or (cpl-impl::task-dead ,task-var)
                                           (not ,invariant)) )
                                ( task
                                  `(cpl-impl::task-dead ,task-var) )
                                ( invariant
                                  `(not ,invariant) )
                                ( T
                                  `(make-fluent :name 'false :value nil) )))
                           ,@(when interval
                             `((deactivate ,recorder-var)
                               (setf ,interval-data-var
                                 (process-recording ,recorder-var :output :data-sig-assoc :action :delete-data)))))
                         ,@(when end
                           `((setf ,end-data-var (list ,@(mapcar #'(lambda (ee) (make-data-list-code ee task-var task))
                                                                 end)))))
                         (add-experience-data ,experience-aut-var
                                              (make-instance 'experience-data
                                                ,@(when begin `(:begin ,begin-data-var))
                                                ,@(when end `(:end ,end-data-var))
                                                ,@(when interval `(:interval ,interval-data-var))))
                         ,@(when (and task (eq (taskspec-type taskspec) :plan))
                           `((remove-requested-task ',(taskspec-label taskspec) ',request-id))))))
                 (task-init-code (when task
                                   (case (taskspec-type taskspec)
                                     ((:plan :tagged)
                                      `((setf ,task-fluent-var (register-task-request ',(taskspec-label taskspec) ',request-id))
                                        (wait-for ,task-fluent-var))))))
                 (child-and-main-code (if children
                                        `(pursue
                                          (seq ,this-automaton-main-code)
                                          ,@child-acquisition-codes)
                                        this-automaton-main-code)) )
            (if parent-experience-aut-var
              `(loop ; code at all levels except top level
                ,@task-init-code
                ,child-and-main-code)
              `(loop ; top-level collection code
                (reset-experience ,experience-aut-var)
                ,@task-init-code
                (unwind-protect
                  ,child-and-main-code
                  (when (data (experience-automaton (getgv :experience ',name)))
                    (deliver-experience (getgv :experience ',name))))))))))))


;; auxiliary functions
(defun make-data-list-code (data-spec task-var task-specified-p)
  (declare (ignorable task-var))
  (labels ( (parse-data-spec (dspec)
              (cond ( (atom dspec)
                      dspec )
                    ( (eq (first dspec) :desig-value)
                      `(desig:desig-prop-value
                        (first (cpl-impl::code-parameters (cpl-impl::task-tree-node-code
                          ,(if task-specified-p
                            `(cpl-impl::task-tree-node (cpl-impl::task-path ,task-var))
                            `cpl-impl::*current-task-tree-node*))))
                        ,(second dspec)) )
                    ( T
                      (cons (first dspec) (mapcar #'parse-data-spec (rest dspec))) ))) )
    `(cons ',(first data-spec)
           (copy-object-values ,(parse-data-spec (second data-spec))))))


(defstruct taskspec
  type
  label)

(defun create-taskspec (taskdef)
  (make-taskspec
    :type (first taskdef)
    :label (second taskdef)))
