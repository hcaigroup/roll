(in-package :roll)

;; integrate learned function
(defmethod parse-weka-output ((ls weka-svm) attributes-with-types)
  (let ( (streamvar (gentemp "STREAM" (package-name (learned-function-package ls)))) )
    `(progn
        ; write test arff file
        (with-open-file (,streamvar ,(namestring (integration-testing-file ls))
                        :direction :output :if-exists :supersede :if-does-not-exist :create)
          ; precompute header of testing files
          (format ,streamvar
            ,(format nil "@relation dummy-relation-name~~2%~{@attribute ~{~a~^ ~}~~%~}~~%@data~~%"
              (convert-attribute-types attributes-with-types))) ; see file weka-learning.lisp
          ; add data into testing file
          (format ,streamvar
            ,(format nil "~~@{~~,5f~~^,~~@} ~a"
              (let ( (target-attribute-type (second (first (last attributes-with-types)))) )
                     (if (listp target-attribute-type)
                       (first target-attribute-type)
                       0)))
            ,@(first (learning-signature ls))))
        ; run weka
        (port:run-prog "java"
          :args ',(append
                  '("weka.classifiers.functions.SMO")
                  `("-T" ,(namestring (integration-testing-file ls)))
                  `("-l" ,(namestring (model-file ls)))
                  `("-p" "0"))
          :output ,(namestring (integration-output-file ls)) :if-output-exists :supersede)
        ; get result
        (with-open-file (,streamvar ,(namestring (integration-output-file ls)))
          (read ,streamvar)
          (read ,streamvar)))))

;; arff and output files are not deleted on purpuse in order to make it not slower than it already is

