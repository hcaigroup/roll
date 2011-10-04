(in-package :roll)

(defgeneric run-weka (learning-system))

(defun convert-attribute-types (attributes)
  ; conversion for enumeration types
  (mapcar #'(lambda (attr) (destructuring-bind (attr-name attr-type) attr
                             (list attr-name
                                   (if (listp attr-type)
                                     (format nil "{~{~a~^, ~}}" attr-type)
                                     attr-type))))
          attributes))

;; do-learning
(defmethod do-learning ((ls weka) (experience weka-experience))
  (ensure-directories-exist (arff-file ls))
  ; finish arff file
    (port:run-prog "/bin/bash"
      :args (list
              "-c"
              (format nil "echo -e \"@relation ~a\\n\\n~{@attribute ~{~a~^ ~}\\n~}\\n@data\"; cat ~a"
                (relation-name experience)
                (convert-attribute-types (attribute-types experience))
                (namestring (arff-tmp-file experience))))
      :output (arff-file ls) :if-output-exists :supersede)
    (port:run-prog "/usr/bin/rm"
      :args (list (namestring (arff-tmp-file experience))))
    (run-weka ls))


;; run-weka

; auxiliary function for parameter specification
(declaim (inline make-parameter-list-for-weka))
(defun make-parameter-list-for-weka (ls ls-accessor weka-parameter)
  (when (funcall ls-accessor ls)
    (list weka-parameter (format nil "~a" (funcall ls-accessor ls)))))


; m5prime: regression and model trees
(defmethod run-weka ((ls weka-m5prime))
  (port:run-prog "/usr/bin/java"
    :args (append
            '("weka.classifiers.trees.M5P")
            (when (use-unpruned-tree ls) '("-N"))
            (when (use-unsmoothed-predictions ls) '("-U"))
            (when (build-regression-tree ls) '("-R"))
            (make-parameter-list-for-weka ls #'minimum-number-of-instances "-M")
            `("-t" ,(namestring (arff-file ls))))
    :output (weka-output-file ls) :if-output-exists :supersede))

; j48: decision trees
(defmethod run-weka ((ls weka-j48))
  (port:run-prog "/usr/bin/java"
    :args (append
            '("weka.classifiers.trees.J48")
            (when (use-unpruned-tree ls) '("-U"))
            (make-parameter-list-for-weka ls #'pruning-confidence-threshold "-C")
            (make-parameter-list-for-weka ls #'minimum-number-of-instances "-M")
            (when (reduced-error-pruning ls) '("-R"))
            (make-parameter-list-for-weka ls #'number-of-folds "-N")
            (when (use-binary-splits-only ls) '("-S"))
            `("-t" ,(namestring (arff-file ls))))
    :output (weka-output-file ls) :if-output-exists :supersede))


; SMO: support vector machines
(defmethod run-weka ((ls weka-svm))
  (port:run-prog "/usr/bin/java"
    :args (append
            '("weka.classifiers.functions.SMO")
            (make-parameter-list-for-weka ls #'complexity-constant "-C")
            (make-parameter-list-for-weka ls #'polynomial-exponent "-E")
            (make-parameter-list-for-weka ls #'rbf-gamma "-G")
            (make-parameter-list-for-weka ls #'normalize-or-standardize "-N")
            (when (normalize-feature-space ls) '("-F"))
            (when (use-lower-order-terms ls) '("-O"))
            (when (use-rbf-kernel ls) '("-R"))
            (make-parameter-list-for-weka ls #'kernel-cache-size "-A")
            (make-parameter-list-for-weka ls #'tolerance-parameter "-L")
            (make-parameter-list-for-weka ls #'epsilon-round-off-error "-P")
            (when (fit-logistic-models ls) '("M"))
            (make-parameter-list-for-weka ls #'folds-for-cross-validation "-V")
            (make-parameter-list-for-weka ls #'random-seed-cross-validation "-W")
            `("-t" ,(namestring (arff-file ls)))
            `("-d" ,(namestring (model-file ls))))
    :output (weka-output-file ls) :if-output-exists :supersede))
