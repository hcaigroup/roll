(in-package :roll)

(defclass experience ()
  ( (name :initarg :name :reader name)
    (experience-automaton :initarg :experience-automaton :accessor experience-automaton) ))


;; storage mode
(defclass transient-experience (experience)
  ( (internal-child-experiences :initform () :reader child-experiences) ))

(defclass persistent-experience (experience) ())

;; raw/abstract experiences
(defclass raw-experience (transient-experience)
  ( (specification-code :initarg :specification-code :initform nil :accessor specification-code) ))

(defclass abstract-experience (experience)
  ( (parent-experiences :initarg :parent-experiences :accessor parent-experiences :initform nil)
    (interal-conversion-specs :initarg :conversion-specs) )
  (:default-initargs
    :conversion-specs (make-hash-table)))

(defclass transient-abstract-experience (abstract-experience transient-experience) ())

(defclass persistent-abstract-experience (abstract-experience persistent-experience) ())

; organize parent-child relationships
(defmethod initialize-instance :after ((exp abstract-experience) &key parent-experiences &allow-other-keys)
  (mapcar #'(lambda (parent-exp)
              (when (typep parent-exp 'transient-experience)
                (push exp (slot-value parent-exp 'internal-child-experiences))))
          parent-experiences))


;;; global structure
(create-global-structure :experience)


;;; generic functions
(defgeneric convert (from to))
(defgeneric deliver-experience (experience))
(defgeneric retrieve-experience (experience &rest conditions))
(defgeneric update-experience (experience))

;;; methods
(defmethod deliver-experience ((experience experience))
  (warn "[EXPERIENCE] No deliver-experience method for class: ~a~%" (type-of experience)))

(defmethod deliver-experience ((experience transient-experience))
  (mapcar #'(lambda (ex) (convert experience ex))
          (child-experiences experience)))

(defmethod retrieve-experience ((experience transient-experience) &rest conditions)
  (declare (ignore conditions))
  experience)

(defmethod update-experience ((experience raw-experience))
  nil)

(defmethod update-experience ((experience abstract-experience))
  (mapcar #'(lambda (ii)
              (unless (typep ii 'transient-experience)
                (update-experience ii)
                (convert ii experience)))
    (parent-experiences experience)))
