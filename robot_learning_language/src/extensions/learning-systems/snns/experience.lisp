(in-package :roll)

; class
(defclass snns-experience (learning-experience)
  ( (experience-count :initform 0 :accessor experience-count)
    (pattern-tmp-file :initform nil :accessor pattern-tmp-file) ))


; set pattern-tmp-file when possible
(defmethod (setf learning-system) (value (experience snns-experience))
  (setf (pattern-tmp-file experience)
        (merge-pathnames (make-pathname :name (cut:string-concat (pathname-name (pattern-file value)) ".tmp"))
                         (pattern-file value)))

  (call-next-method))

; update-experience
(defmethod update-experience :before ((experience snns-experience))
  (port:run-prog "/bin/rm"
    :args (list "-f" (namestring (pattern-tmp-file experience)))))

; deliver-experience (write pattern data)
(defmethod deliver-experience ((experience snns-experience))
  (ensure-directories-exist (pattern-tmp-file experience))
  (let ( (begin-data (get-experience-data (experience-automaton experience)
                                          :begin :occurrence-content :all-occurrences))
         (end-data (get-experience-data (experience-automaton experience)
                                        :end :occurrence-content :all-occurrences)) )
    (when (and begin-data end-data)
      ; empty begin-data or end-data can happen if an episode exists, but a specific automaton did not have any data for this episode. Would be nicer to treat in convert, but I try to avoid to change that code unnecessarily
      (with-open-file (stream (pattern-tmp-file experience)
                       :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream "~{~{~{~,5f ~}~{~,5f~^ ~}~}~^~%~}~%"
          (mapcar #'list
            (mapcar #'(lambda (x) (mapcar #'cdr x)) (first begin-data))
            (mapcar #'(lambda (x) (mapcar #'cdr x)) (first end-data)))))
      (setf (experience-count experience) (+ (experience-count experience) (length begin-data))))))
