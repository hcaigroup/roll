(in-package :roll)

(let ( (episode-numbers :empty) )

  (defmethod retrieve-experience ((experience database-experience) &rest conditions)
    (declare (ignore conditions))
    (when (eq episode-numbers :empty)
      (setf episode-numbers
        (alexandria:flatten (execute-db-query experience
                                              (format nil "SELECT `EPISODE-NR` FROM `~a`"
                                                (create-table-name experience :management))))))
    (let ( (episode-number (pop episode-numbers)) )
      (cond ( episode-number
              (reset-experience (experience-automaton experience))
              (fill-experience-automaton (experience-automaton experience) episode-number experience)
              experience )
            ( T ; when all episodes retrieved, reset variable for further updates
              (setf episode-numbers :empty)
              nil ))))

)


(defun fill-experience-automaton (exp-automaton episode-number db-experience)
  ;; auxiliary functions
  (labels ( ; make SQL column specifications for each event type and organizational columns
            (make-column-specs (lst &optional prefix)
              (when lst
                (list
                  (format nil
                    (if prefix
                      (format nil "~~{~a.`~~a`~~^, ~~}" prefix)
                      "~{`~a`~^, ~}")
                    lst))))
            ; fit db data to signature to construct experience data
            (construct-experience-data-from-db-output (db-output signature)
              (if (or (null signature) (null db-output))
                (values nil db-output)
                (multiple-value-bind (exp-data-result rest-db-output)
                                     (construct-experience-data-from-db-output (rest db-output) (rest signature))
                  (values
                    (cons (cons (first signature) (read-lisp-type-if-necessary (first db-output))) exp-data-result)
                    rest-db-output)))) )
    (let ( (signature (signature exp-automaton))
           (autname (name exp-automaton))
           tables )
      ; get table names
      (when (begin signature)
        (push (cons "tb" (create-table-name db-experience :begin autname)) tables))
      (when (end signature)
        (push (cons "te" (create-table-name db-experience :end autname)) tables))
      (when (interval signature)
        (push (cons "ti" (create-table-name db-experience :interval autname)) tables))
      ; db query
      (let ( (db-result (execute-db-query db-experience
                          (format nil "SELECT ~{~a ~^,~} FROM ~{~a ~^JOIN ~} ~a WHERE ~a.`EPISODE-NR`=~a"
                            (mapcan #'(lambda (x) (make-column-specs (car x) (cdr x)))
                                    (list
                                      (cons (begin signature) "tb")
                                      (cons (end signature) "te")
                                      (cons (interval signature) "ti")
                                      (cons (make-columns-for-level (level exp-automaton)) (caar tables))))
                            (mapcar #'(lambda (tab) (format nil "`~a` ~a " (cdr tab) (car tab))) tables)
                            (if (or (null tables) (null (rest tables)))
                              ""
                              (format nil "ON ~{~a ~^AND ~}"
                                (mapcan #'(lambda (t1 t2)
                                            (mapcar #'(lambda (occ-column)
                                                        (format nil "~a.`~a`=~a.`~a`" (car t1) occ-column (car t2) occ-column))
                                                    (cons "EPISODE-NR" (make-columns-for-level (level exp-automaton)))))
                                        tables
                                        (rest tables))))
                            (caar tables)
                            episode-number))) )
      ; convert result to experience data
      (dolist (db-res db-result)
        (multiple-value-bind (exp-data-begin rest-begin-db-data)
                             (construct-experience-data-from-db-output db-res (begin signature))
          (multiple-value-bind (exp-data-end rest-end-db-data)
                               (construct-experience-data-from-db-output rest-begin-db-data (end signature))
            (multiple-value-bind (exp-data-interval rest-interval-db-data)
                                 (construct-experience-data-from-db-output rest-end-db-data (interval signature))
              (add-experience-data exp-automaton
                                   (make-instance 'experience-data
                                     :begin exp-data-begin
                                     :end exp-data-end
                                     :interval exp-data-interval)
                                   rest-interval-db-data))))))))
  ; fill child automata
  (mapcar #'(lambda (child) (fill-experience-automaton child episode-number db-experience))
            (children exp-automaton)))



