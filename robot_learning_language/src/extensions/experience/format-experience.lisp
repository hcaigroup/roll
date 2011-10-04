(in-package :roll)

(defclass format-experience (abstract-experience) ())

(defmethod deliver-experience ((experience format-experience))
  (format t "~%-----------------------------")
  (format t "~%data of experience ~a:"
    (name experience))
  (output-experience-automaton (experience-automaton experience))
  (format t "~%-----------------------------~2%"))

(defun output-experience-automaton (exp-aut)
  (format t "~%automaton ~a:~%begin: ~a~%end: ~a~%interval: ~a"
    (name exp-aut)
    (get-experience-data exp-aut :begin)
    (get-experience-data exp-aut :end)
    (get-experience-data exp-aut :interval))
  (dolist (cc (children exp-aut))
    (output-experience-automaton cc)))
