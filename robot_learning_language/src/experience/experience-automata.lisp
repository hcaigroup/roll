(in-package :roll)

(defparameter *default-automaton-name* 'default-automaton)

;; --------------------------------------------------------- ;;
;; -- Classes ---------------------------------------------- ;;
;; --------------------------------------------------------- ;;

(defclass experience-automaton ()
  ( (name :initform nil :initarg :name :reader name)
    (level :initform 0 :accessor level)
    (parent :initform nil :reader parent)
    (children :initform nil :initarg :children :reader children)
    (data :initform nil :accessor data)
    (signature :reader signature)
    (occurrence-prefix :initform nil :accessor occurrence-prefix) ))

(defmethod initialize-instance :after ((exa experience-automaton) &key signature &allow-other-keys)
  (mapcar #'(lambda (child)
             (setf (slot-value child 'parent) exa)
             (setf (level child) 1)
             (push 0 (occurrence-prefix child)))
          (children exa))
  (setf (slot-value exa 'signature) (apply #'make-instance 'experience-data signature)))

(defmethod (setf level) :after (value (exa experience-automaton))
  (mapcar #'(lambda (child) (setf (level child) (1+ value))) (children exa)))

; (setf occurrence-prefix) necessary for organization of occurrence prefixes
(defmethod (setf occurrence-prefix) :after (value (exa experience-automaton))
  (mapcar #'(lambda (child) (setf (occurrence-prefix child) (cons 0 value)))
          (children exa)))


(defclass experience-data ()
  ( (begin :initform nil :initarg :begin :accessor begin)
    (end :initform nil :initarg :end :accessor end)
    (interval :initform nil :initarg :interval :accessor interval) ))


;; --------------------------------------------------------- ;;
;; -- Generic Functions ------------------------------------ ;;
;; --------------------------------------------------------- ;;
(defgeneric reset-experience (experience-automaton))
(defgeneric get-experience-event-data (experience-data event &optional var))
(defgeneric get-experience-data (experience-automaton event &key occurrence-content var))
(defgeneric get-experience-signature (experience-automaton event))
(defgeneric add-experience-data (experience-automaton experience-data &optional occurrence-spec))
(defgeneric find-sub-automaton (experience-automaton autname))
(defgeneric sort-occurrences (experience-automaton &optional autname))

;; --------------------------------------------------------- ;;
;; -- Methods ---------------------------------------------- ;;
;; --------------------------------------------------------- ;;

;; resetting axperience automata
(defmethod reset-experience ((exa experience-automaton))
  (setf (data exa) nil)
  (setf (occurrence-prefix exa) (mapcar #'(lambda (x) (declare (ignore x)) 0) (occurrence-prefix exa)))
  (mapcar #'reset-experience (children exa)))

;; reading data

(defmethod get-experience-event-data ((exd experience-data) slot-keyword &optional var)
  (let ( (slot-val (funcall (symbol-function (intern (symbol-name slot-keyword) :roll)) exd)) )
    (if var
      (cdr (assoc var slot-val))
      slot-val)))

(defmethod get-experience-data ((exa experience-automaton) event &key (occurrence-content :all-occurrences) var)
; event: begin/end/interval
; occurrence-content: :all-occurrences, :only-occurrence, occurrence-index, ...
; ohne Angabe von var: all-vars
  (if (eq occurrence-content :all-occurrences)
    (mapcar #'(lambda (dat) (cons (get-experience-event-data (first dat) event var) (rest dat))) (data exa))
    (get-experience-event-data
      (first (cond ( (eq occurrence-content :only-occurrence)
                     (first (data exa)) )
                   ( (eq occurrence-content :first-occurrence)
                     (first (last (data exa))) )
                   ( (eq occurrence-content :last-occurrence)
                     (first (data exa)) )
                   ( (listp occurrence-content)
                     (find occurrence-content (data exa) :key #'rest :test #'equal) )))
      event
      var)))

(defmethod get-experience-signature ((exa experience-automaton) event)
  (get-experience-event-data (signature exa) event))


;; writing data
(defmethod add-experience-data ((exa experience-automaton) (data experience-data) &optional occurrence-spec)
  (push (cons data (or occurrence-spec (occurrence-prefix exa)))
        (data exa))
  (when (and (plusp (level exa)) (not occurrence-spec))
    (setf (occurrence-prefix exa)
          (cons (1+ (first (occurrence-prefix exa))) (rest (occurrence-prefix exa)))))
  exa)

;; find child automaton
(defmethod find-sub-automaton ((exa experience-automaton) autname)
  (if (eq (name exa) autname)
    exa
    (find-if #'(lambda (x) (not (null x)))
             (mapcar #'(lambda (aut) (find-sub-automaton aut autname)) (children exa)))))

;; sort occurrences
(defmethod sort-occurrences ((exa experience-automaton) &optional autname)
  (let ( (automaton (if autname
                      (find-sub-automaton exa autname)
                      exa)) )
    (setf (data automaton)
          (sort
            (data automaton)
            #'compare-occurrence-specs
            :key #'cdr))
    (unless autname
      (dolist (cc (children exa))
        (sort-occurrences cc)))))

; compare-occurrence-specs
; could be made more efficient by first reversing both lists and then having the higher levels in front of the list
; but occurrence lists will hardly ever be longer than three elements, so there is not much to gain
(defun compare-occurrence-specs (os1 os2)
  (cond ( (and (numberp os1) (numberp os2))
          (> os1 os2) )
        ( (or (and (listp os1) (numberp os2))
              (and (numberp os1) (listp os2))
              (/= (length os1) (length os2)))
          (error "[EXPERIENCE AUTOMATON] can't compare occurrence specs on different levels: ~a and ~a" os1 os2) )
        ( (and (null os1) (null os2))
          T )
        ( T
          (let ( (os1-level0 (alexandria:last-elt os1))
                 (os2-level0 (alexandria:last-elt os2)) )
            (cond ( (> os1-level0 os2-level0) T )
                  ( (< os1-level0 os2-level0) nil )
                  ( T
                    (compare-occurrence-specs (butlast os1) (butlast os2)) ))) )))

;; order experience data by signature
; used in construct-exp-data-occurrences to reconstruct order of signature
(defun order-experience-data (exd signature)
  (setf (begin exd) (sort-list-by-signature (begin exd) (begin signature)))
  (setf (end exd) (sort-list-by-signature (end exd) (end signature)))
  (setf (interval exd) (sort-list-by-signature (interval exd) (interval signature)))
  exd)

(defun sort-list-by-signature (input-list signature-list)
  (sort input-list
        #'(lambda (x y) (occurs-before x y signature-list))
        :key #'first))

(defun occurs-before (x y seq &key (test #'eql))
  (cond ( (null seq)
          (warn "[EXPERIENCE AUTOMATA UTILITY] values ~a and ~a not in sequence ~a" x y seq)
          nil ) ; actually value is undefined
        ( (funcall test x (first seq))
          T )
        ( (funcall test y (first seq))
          nil )
        ( T
          (occurs-before x y (rest seq) :test test) )))


