(in-package :roll)

;; classes
(defclass database ()
  ( (db-host :initarg :host :reader get-db-host :writer set-db-host)
    (db-user :initarg :user :reader get-db-user :writer set-db-user)
    (db-user-pw :initarg :user-pw :reader get-db-user-pw :writer set-db-user-pw)
    (db-name :initarg :name :reader get-db-name :writer set-db-name) ))

(defclass mysql-database (database) ())

;; generic functions
(defgeneric make-database-spec (database &key db-source))
(defgeneric clear-database (database))

;; methods

; make-database-spec
(defmethod make-database-spec ((db database) &key (db-source :clsql))
  (case db-source
    ( :clsql
      (error "[DATABASE] CLSQL database type ~s not supported." (type-of db)) )
    ( :port
      (list "-u" (get-db-user db)
            "-h" (get-db-host db)
            (format nil "-p~a" (get-db-user-pw db))
            (get-db-name db)) )))

(defmethod make-database-spec ((db mysql-database) &key (db-source :clsql))
  (case db-source
    ( :clsql
      (list (get-db-host db) (get-db-name db) (get-db-user db) (get-db-user-pw db)) )
    ( :port
      (call-next-method) )))


; clear-database
(defmethod clear-database ((db database))
  (clsql:with-database (open-db (make-database-spec db :db-source :clsql))
    (mapcar #'(lambda (table)
                (clsql:execute-command (format nil "drop table `~a`" table) :database open-db))
                 ;(clsql:drop-table table :database db)) -> hat nicht immer funktioniert
            (clsql:list-tables :database open-db))))

; type conversions
(defun sql-type-of (tp)
  (cond ( (or (typep tp 'bignum)
              (and (typep tp 'integer) (> (abs tp) 2147483647)))
          "BIGINT" )
        ( (typep tp 'integer)
          "INT" )
        ( (typep tp 'real)
          "DECIMAL(10,5)" )
        ( (or (eq tp T) (eq tp nil))
          "TINYINT" )
        ( (typep tp 'standard-object)
          (warn "[DATABASE] Type standard-object of object ~s cannot be converted to SQL type" tp)
          "VARCHAR(127)" )
        ( T
          "VARCHAR(255)" )))


(defun convert-lisp-to-sql-type (input &optional (sql-type (sql-type-of input)))
  (cond ( (string-equal sql-type "TINYINT")
          (if input 1 0) )
        ( (or (member sql-type '("BIGINT" "INT") :test #'string-equal)
              (= (string<= "DECIMAL" sql-type) 7)
              (= (string<= "FLOAT" sql-type) 5))
          input )
        ( T
          (format nil "~a" input) )))

; this function is only necessary when using CLSQL with SBCL:
; if the value is a decimal, CLSQL queries return a string instead of a number
(defun read-lisp-type-if-necessary (val)
  (if (typep val 'string)
    (read-from-string val)
    val))

; this function is an auxiliary to write strings into the DB
; in case a string should be stored it should be packed into another string, so that
; read-lisp-type-if-necessary returns a string instead of trying to parse the data
(defun make-db-string (val)
  (format nil "\"~a\"" val))


