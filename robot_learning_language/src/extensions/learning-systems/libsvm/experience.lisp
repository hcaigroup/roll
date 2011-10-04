(in-package :roll)

; class
(defclass libsvm-experience (learning-experience)
  ( (libsvm-target-data :initform () :accessor libsvm-target-data)
    (libsvm-input-data :initform () :accessor libsvm-input-data) ))

(defmethod update-experience :before ((experience libsvm-experience))
  (setf (libsvm-target-data experience) ())
  (setf (libsvm-input-data experience) ()))

; deliver-experience
(defmethod deliver-experience ((experience libsvm-experience))
  (let ( (begin-data (get-experience-data (experience-automaton experience)
                                          :begin :occurrence-content :all-occurrences))
         (end-data (get-experience-data (experience-automaton experience)
                                        :end :occurrence-content :all-occurrences)) )
    (when (and begin-data end-data)
      ; empty begin-data or end-data can happen if an episode exists, but a specific automaton did not have any data for this episode. Would be nicer to treat in convert, but I try to avoid to change that code unnecessarily
      (mapcar #'(lambda (dat) (push (first dat) (libsvm-target-data experience)))
              (mapcar #'(lambda (x) (mapcar #'cdr x)) (first end-data)))
      (mapcar #'(lambda (dat) (push (let ( (counter 0)
                                           (input-vector (make-array (length (get-experience-signature
                                                                               (experience-automaton experience)
                                                                               :begin))
                                                                     :fill-pointer 0)) )
                                       (dolist (dd dat input-vector)
                                         (incf counter)
                                         (vector-push (cons counter dd) input-vector)))
                                   (libsvm-input-data experience)))
              (mapcar #'(lambda (x) (mapcar #'cdr x)) (first begin-data))))))

