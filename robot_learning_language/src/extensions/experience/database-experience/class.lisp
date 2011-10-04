(in-package :roll)

(defclass database-experience (persistent-abstract-experience)
  ( (table-name-prefix :accessor table-name-prefix)
    (internal-database-spec :accessor database-spec)
    (internal-db-signature-known :accessor db-signature-known :initform nil)
    (internal-max-episode-number :accessor max-episode-number :initform nil) ))


(defclass db-experience-automaton (experience-automaton)
  ( (db-signature :initarg :db-signature :initform nil :accessor db-signature)
    (db-insert-commands :accessor db-insert-commands) ))


(defmethod initialize-instance :after ((experience database-experience) &key database (table-name-prefix nil tnpp))
  (if database
    (setf (database-spec experience) (make-database-spec database :db-source :clsql))
    (warn "[DATABASE-EXPERIENCE] No database given for experience ~a" (name experience)))
  (setf (table-name-prefix experience)
    (cut:->string (if tnpp table-name-prefix (name experience)))))

(set-automaton-type-for-experience-class 'database-experience 'db-experience-automaton)

;; auxiliary functions
(defun create-table-name (experience table-type &optional automaton-name)
  (string-upcase
    (if (eq table-type :management)
      (cut:string-concat (table-name-prefix experience) "-time-management")
      (format nil "~a~a-~a"
        (table-name-prefix experience)
        (if automaton-name
          (format nil "-~a" automaton-name)
          "")
        table-type))))


; execute-db-command
(defun execute-db-command (db-experience cmd-string)
;  (format t "execute DB command: ~s~%" cmd-string)
  (clsql:with-database (db (database-spec db-experience) :if-exists :new)
    (clsql:execute-command cmd-string :database db)))

; execute-db-query
(defun execute-db-query (db-experience cmd-string)
;  (format t "execute DB query: ~s~%" cmd-string)
  (clsql:with-database (db (database-spec db-experience))
    (clsql:query cmd-string :database db)))
