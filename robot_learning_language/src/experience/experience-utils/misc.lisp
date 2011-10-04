(in-package :roll)

(defun make-experience-slot-definitions (automaton &key parent (exp-data-class 'exp-data))
  (labels ( (get-sig (spec)
              (if (atom (first spec))
                spec
                (mapcar #'first spec))) )
    (destructuring-bind (name &key begin end interval children &allow-other-keys) automaton
      (cons
        `(,name :initform (make-instance 'experience-data
                            ,@(when begin
                              `(:begin (make-instance ',exp-data-class :signature ',(get-sig begin))))
                            ,@(when end
                              `(:end (make-instance ',exp-data-class :signature ',(get-sig end))))
                            ,@(when interval
                              `(:interval (make-instance ',exp-data-class :signature ',(get-sig interval)))))
                :accessor ,name
                :parent ,parent
                :is-experience-slot T)
        (mapcan #'(lambda (cc) (make-experience-slot-definitions cc :parent name :exp-data-class exp-data-class))
                children)))))

;; copy functions
(defgeneric copy-object-parsimoniously (obj &key ignore-slots filter))
(defgeneric copy-content-parsimoniously (from to &key ignore-slots filter))

; copy-object-parsimoniously
(defmethod copy-object-parsimoniously (var &key ignore-slots (filter #'identity))
  (declare (ignore ignore-slots))
  (let ( (var-filter (funcall filter var)) )
    (cond ( (typep var 'fluent)
            (value var-filter) )
          ( T
            var-filter ))))

(defmethod copy-object-parsimoniously ((var standard-object) &key ignore-slots (filter #'identity))
  (copy-content-parsimoniously
    (funcall filter var)
    (allocate-instance (class-of (funcall filter var)))
    :ignore-slots ignore-slots
    :filter filter))

; copy-content-parsimoniously
(defmethod copy-content-parsimoniously (from to &key ignore-slots (filter #'identity))
  (declare (ignore ignore-slots))
  (declare (ignore to))
  (funcall filter from))

(defmethod copy-content-parsimoniously ((from standard-object) (to standard-object) &key ignore-slots (filter #'identity))
  (mapcar #'(lambda (slot-name)
              (when (and (slot-boundp from slot-name)
                         (not (member slot-name ignore-slots)))
                (let ( (from-slot (let ( (from-filter (funcall filter (slot-value from slot-name))) )
                                    (if (typep from-filter 'fluent)
                                      (value from-filter)
                                      from-filter))) )
                  (cond ( (typep from-slot 'standard-object)
                          (if (and (slot-boundp to slot-name)
                                   (eq (type-of from-slot) (type-of (slot-value to slot-name))))
                            (copy-content-parsimoniously from-slot (slot-value to slot-name)
                              :ignore-slots ignore-slots
                              :filter filter)
                            (setf (slot-value to slot-name)
                                  (copy-object-parsimoniously from-slot :ignore-slots ignore-slots :filter filter))) )
                        ( T
                          (setf (slot-value to slot-name) from-slot) )))))
          (intersection
            (mapcar #'clos:slot-definition-name (clos:class-slots (class-of from)))
            (mapcar #'clos:slot-definition-name (clos:class-slots (class-of to)))))
  to)

; copy object values
(defun copy-object-values (var &key ignore-slots)
  (copy-object-parsimoniously var :ignore-slots ignore-slots :filter #'value))
