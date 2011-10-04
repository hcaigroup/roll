(in-package :roll)

;; deliver-experience method
(defmethod deliver-experience :before ((experience database-experience))
  (let ( db-tables )
    (labels ( ; get-table-signature
              ; -------------------
              ; for each event in an automaton:
              ; 1. get DB signature:
              ; 1a) if table exists, extract DB signature from DB
              ; 1b) if table doesn't exist, get signature from data and create table
              ; 2. generate insert statement for respective table
              (get-table-signature (experience-automaton event special-columns)
                (when (get-experience-signature experience-automaton event)
                  (let*( (table-name (create-table-name experience event (name experience-automaton)))
                         (db-sig (cond ( (find table-name db-tables :test #'string-equal)
                                         (let ( (table-vars (execute-db-query experience
                                                              (format nil "SHOW COLUMNS IN `~a`" table-name))) )
                                           (mapcar #'(lambda (var)
                                                       (cons var
                                                         (string-upcase
                                                           (cadr
                                                             (find var table-vars
                                                               :key #'first
                                                               :test #'(lambda (symb str)
                                                                               (string-equal (symbol-name symb) str)))))))
                                                   (get-experience-signature experience-automaton event))) )
                                       ( T
                                         (let ( (sig (mapcar #'(lambda (dat)
                                                                 (cons (car dat)
                                                                       (sql-type-of
                                                                         (if (eq event :interval)
                                                                           (caddr dat)
                                                                           (cdr dat)))))
                                                             (get-experience-data experience-automaton
                                                               event
                                                               :occurrence-content :first-occurrence))) )
                                           (execute-db-command experience
                                             (format nil
                                               "CREATE TABLE IF NOT EXISTS `~a`
                                                 (~{`~a` INT UNSIGNED NOT NULL, ~}~{~{`~a` ~a, ~}~}PRIMARY KEY(~{`~a`~^, ~}));"
                                               table-name
                                               special-columns
                                               (mapcar #'(lambda (cc) (list (car cc) (cdr cc))) sig)
                                               special-columns))
                                           sig) ))) )
                    (values
                      db-sig
                      ; generate insert statement
                      (format nil "INSERT INTO `~a` (~{`~a`, ~}~{`~a`~^, ~}) VALUES (~~{~{~a, ~}~~}~~{~{~a~^, ~}~~})"
                        table-name
                        special-columns
                        (mapcar #'first db-sig)
                        (mapcar #'(lambda (x) (declare (ignore x)) "~s") special-columns)
                        (mapcar #'(lambda (x) (if (cut:string-prefix-p "DECIMAL" (cdr x)) "~,5f" "~s"))
                                db-sig))))))
              (retrieve-signature-from-db (experience-automaton)
                (unless db-tables
                  (setf db-tables
                        (clsql:with-database (db (database-spec experience))
                          (clsql:list-tables :database db))))
                (let ( (special-columns (cons "EPISODE-NR"
                                              (make-columns-for-level (level experience-automaton)))) )
                  (multiple-value-bind (begin-db-sig begin-insert-command)
                                         (get-table-signature experience-automaton :begin special-columns)
                  (multiple-value-bind (end-db-sig end-insert-command)
                                         (get-table-signature experience-automaton :end special-columns)
                  (multiple-value-bind (interval-db-sig interval-insert-command)
                                         (get-table-signature experience-automaton :interval
                                           (cons "EPISODE-NR"
                                                 (append (make-columns-for-level (level experience-automaton))
                                                         '("INSTANCE-NR"))))
                    (setf (db-signature experience-automaton)
                          (make-instance 'experience-data
                            :begin begin-db-sig
                            :end end-db-sig
                            :interval interval-db-sig))
                    (setf (db-insert-commands experience-automaton)
                          (make-instance 'experience-data
                            :begin begin-insert-command
                            :end end-insert-command
                            :interval interval-insert-command))))))
                (mapcar #'retrieve-signature-from-db (children experience-automaton))) )
    ; set signature and SQL commands
    (unless (db-signature-known experience)
      (retrieve-signature-from-db (experience-automaton experience))
      (setf (db-signature-known experience) T)
      ; create management table if necessary
      (let ( (mgmt-table-name (create-table-name experience :management)) )
        (unless (find mgmt-table-name db-tables :test #'string-equal)
          (execute-db-command experience
            (format nil "CREATE TABLE IF NOT EXISTS `~a`
                        (`EPISODE-NR` INT UNSIGNED PRIMARY KEY NOT NULL, `TIME` INT UNSIGNED NOT NULL)"
                    mgmt-table-name))
          (setf (max-episode-number experience) 0))
        ; get maximum episode number from database
        (unless (max-episode-number experience)
          (setf (max-episode-number experience)
                (caar (execute-db-query
                        experience
                        (format nil "SELECT MAX(`EPISODE-NR`) FROM `~a`" mgmt-table-name))))))))))

(defun make-columns-for-level (n)
  (if (zerop n)
    nil
    (cons (format nil "OCCURRENCE-LEVEL-~a" n) (make-columns-for-level (1- n)))))

; main method
(defmethod deliver-experience ((experience database-experience))
  ;; update management-tabelle nicht vergessen!
  (labels ( (insert-automaton-data-into-db (automaton)
              (dolist (ee '(:begin :end :interval))
                (insert-automaton-event-data-into-db automaton ee))
              (mapcar #'insert-automaton-data-into-db (children automaton)))
            (insert-automaton-event-data-into-db (automaton event)
              (when (get-experience-signature automaton event)
                (let ( (exp-data (get-experience-data automaton event :occurrence-content :all-occurrences))
                       (insert-template (get-experience-event-data (db-insert-commands automaton) event)) )
                  (dolist (ee exp-data) ; insert new line for each occurrence
                    (destructuring-bind (data-list &rest occurrence-spec-1) ee
                      (let ( (occurrence-spec (alexandria:ensure-list occurrence-spec-1)) )
                        (if (eq event :interval) ; interval with occurrences not tested yet!
                          (let ( (instance-count 0) )
                            (dolist (ii (cdr (apply #'mapcar #'list data-list)))
                              (incf instance-count)
                              (execute-db-command
                                experience
                                (format nil insert-template
                                  (cons (max-episode-number experience)
                                        (reverse (cons instance-count occurrence-spec)))
                                  ii))))
                          (execute-db-command
                            experience
                            (format nil insert-template
                                    (cons (max-episode-number experience) (reverse occurrence-spec))
                                    (mapcar #'cdr data-list)))))))))) )
    (incf (max-episode-number experience))
    (execute-db-command experience
      (format nil "INSERT INTO `~a` (`EPISODE-NR`, `TIME`) VALUES (~a, ~a)"
        (create-table-name experience :management)
        (max-episode-number experience)
        (get-universal-time))) ;; Zeitstempel nicht ideal, das ist der Zeitpunkt der Umwandlung, nicht des Sammelns
    (insert-automaton-data-into-db (experience-automaton experience))))


