(in-package :roll)

;;; könnte man schöner machen, indem man nur Beispile updated, die nach dem letzten Update beobachtet bzw. umgewandelt wurden

;; update-experience method
(defmethod update-experience :before ((experience database-experience))
  (when (every #'(lambda (p-exp) (typep p-exp 'roll::persistent-abstract-experience))
                (parent-experiences experience))
    ; delete tables from database (TRUNCATE only works if table exists for sure)
    (clsql:with-database (db (database-spec experience) :if-exists :new)
      (mapcar #'(lambda (table-description)
                  (clsql::execute-command
                    (format nil "DROP TABLE IF EXISTS `~a`" table-description)
                    :database db))
              (cons
                (create-table-name experience :management)
                (get-experience-tables (experience-automaton experience) experience))))
    ; reset variables in experience to make sure initialization of deliver-experience works
    (setf (db-signature-known experience) nil)
    (setf (max-episode-number experience) nil)))


(defun get-experience-tables (exp-automaton experience)
  (cons (create-table-name experience :begin (name exp-automaton))
        (cons (create-table-name experience :end (name exp-automaton))
              (cons (create-table-name experience :interval (name exp-automaton))
                    (mapcan #'(lambda (aut) (get-experience-tables aut experience)) (children exp-automaton))))))
