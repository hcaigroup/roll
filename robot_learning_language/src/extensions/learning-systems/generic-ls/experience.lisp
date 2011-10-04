(in-package :roll)


; class
(defclass generic-learning-experience (learning-experience)
  ( (begin-experience-list :initform () :accessor begin-experience-list)
    (end-experience-list :initform () :accessor end-experience-list) ))



; deliver-experience (write pattern data)
(defmethod deliver-experience ((experience generic-learning-experience))
  (setf (begin-experience-list experience)
        (append (get-experience-data (experience-automaton experience)
                                     :begin :occurrence-content :all-occurrences)
                (begin-experience-list experience)))
  (setf (begin-experience-list experience)
        (append (get-experience-data (experience-automaton experience)
                                     :end :occurrence-content :all-occurrences)
                (end-experience-list experience))))

