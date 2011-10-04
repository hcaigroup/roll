(in-package :roll)

(let ( (convert-functions (make-hash-table))
       (conversion-specs (make-hash-table)) )
  ;; store converstion functions and conversion specifications in nested hash tables
  ;; the names of the experiences are used as keys

  (defun add-fun-or-spec (table from-exp to-exp content)
    (let*( (from-exp-entry (gethash from-exp table))
           (from-exp-entry-final (or from-exp-entry
                                 (setf (gethash from-exp table) (make-hash-table)))) )
      (setf (gethash to-exp from-exp-entry-final) content)))

  (defun add-convert-function (from-exp to-exp fun)
    (add-fun-or-spec convert-functions from-exp to-exp fun))

  (defun add-conversion-spec (from-exp to-exp spec)
    (add-fun-or-spec conversion-specs from-exp to-exp spec))


  (defun get-fun-or-spec (table from-exp to-exp)
    (let ( (from-exp-entry (gethash from-exp table)) )
      (if from-exp-entry
        (gethash to-exp from-exp-entry)
        (error "[EXPERIENCE CONVERSION] No entry stored for experiences ~a and ~a"
          from-exp to-exp))))

  (defun get-convert-function (from-exp to-exp)
    (get-fun-or-spec convert-functions from-exp to-exp))

  (defun get-conversion-spec (from-exp to-exp)
    (get-fun-or-spec conversion-specs from-exp to-exp))


  (defun show-fun-or-specs (table)
    (maphash #'(lambda (key val)
              (format t "raw experience: ~a, abstract experiences: ~{~a~^, ~}~%"
                key
                (let ( abstract-experiences )
                  (maphash #'(lambda (k v) (declare (ignore v)) (push k abstract-experiences)) val)
                  abstract-experiences)))
             table))

  (defun show-convert-functions ()
    (show-fun-or-specs convert-functions))

  (defun show-converstion-specs ()
    (show-fun-or-specs conversion-specs))

)

(defmethod convert ((from transient-experience) (to abstract-experience))
  (reset-experience (experience-automaton to))
  (funcall (get-convert-function (name from) (name to)))
  (deliver-experience to))
  ; etwas vereinfacht, wahrscheinlich muss deliver zwischendurch bei convert manchmal aufgerufen werden

(defmethod convert ((from persistent-experience) (to abstract-experience))
  (loop while (retrieve-experience from) do
    (reset-experience (experience-automaton to))
    (funcall (get-convert-function (name from) (name to)))
    (deliver-experience to)))
  ; etwas vereinfacht, wahrscheinlich muss deliver zwischendurch bei convert manchmal aufgerufen werden

;; define-experience-conversion
(defmacro define-experience-conversion (&key from-experience to-experience operations conversion-code)
  `(progn
    (add-conversion-spec ',from-experience ',to-experience ',operations)
    (add-convert-function ',from-experience ',to-experience
                           #'(lambda ()
                             (let ( (,(get-from-experience-var)
                                      (experience-automaton (getgv :experience ',from-experience))) )
                               (when (data ,(get-from-experience-var))
                                 ,@(or conversion-code
                                       (analyze-abstract-experience-definition
                                         operations from-experience
                                         (get-automaton-type-for-experience-class
                                           (type-of (getgv :experience from-experience)))))))))))





