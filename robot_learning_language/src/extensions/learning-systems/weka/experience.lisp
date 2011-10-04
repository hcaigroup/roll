(in-package :roll)

; class
(defclass weka-experience (learning-experience)
  ( (arff-tmp-file :initform nil :accessor arff-tmp-file)
    (relation-name  :initarg :relation-name :initform nil :accessor relation-name)
    (attribute-types :initform nil :accessor attribute-types :initarg :attribute-types) ))


; set arff-tmp-file when possible
(defmethod (setf learning-system) (value (experience weka-experience))
  (setf (arff-tmp-file experience)
        (merge-pathnames (make-pathname :name (cut:string-concat (pathname-name (arff-file value)) ".tmp"))
                         (arff-file value)))
  (unless (relation-name experience)
    (setf (relation-name experience) (pathname-name (arff-file value))))
  (call-next-method))

; update-experience
(defmethod update-experience :before ((experience weka-experience))
  (port:run-prog "/bin/rm"
    :args (list "-f" (namestring (arff-tmp-file experience)))))

; deliver-experience
(defmethod deliver-experience ((experience weka-experience))
  (ensure-directories-exist (arff-tmp-file experience))
  (let ( (begin-data (get-experience-data (experience-automaton experience)
                                          :begin :occurrence-content :all-occurrences))
         (end-data (get-experience-data (experience-automaton experience)
                                        :end :occurrence-content :all-occurrences)) )
    (when (and begin-data end-data)
      ; empty begin-data or end-data can happen if an episode exists, but a specific automaton did not have any data for this episode. Would be nicer to treat in convert, but I try to avoid to change that code unnecessarily
      (with-open-file (stream (arff-tmp-file experience)
                       :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream "~{~{~{~,5f,~}~{~,5f~^,~}~}~^~%~}~%"
          (mapcar #'list
            (mapcar #'(lambda (x) (mapcar #'cdr x)) (first begin-data))
            (mapcar #'(lambda (x) (mapcar #'cdr x)) (first end-data))))))))

