(defpackage roll
  (:nicknames :roll)
  (:use #:cpl)
  (:export
    ; learning-problem-class
    "CREATE-LP-NAME-FOR-FUNCTION-SPEC"
    ; learning-problem
    "DEFINE-LEARNING-PROBLEM" "LEARN"
    ;; EXPERIENCES
    ; problem-generation
    "GENERATE-PARAMETER-SAMPLES" "WITH-PARAMETER-SAMPLES"
    ; raw-experience
    "DEFINE-RAW-EXPERIENCE"
    ; abstract-experience
    "DEFINE-ABSTRACT-EXPERIENCE" "DEFINE-EXPERIENCE-CONVERSION"
    "WITH-CROSS-PRODUCT" "WITH-BINDING" "WITH-FILTER"
    "TRANSIENT-ABSTRACT-EXPERIENCE"
    ; experience-utils
    "AGGREGATE-OCCURRENCES" "MAP-OCCURRENCES" "FILTER-OCCURRENCES"
    ; input-conversion
    "CONVERSION-FROM"
    ;; EXTENSIONS
    ; experiences
    "FORMAT-EXPERIENCE"
    "DATABASE-EXPERIENCE" "FRUGAL-DATABASE-EXPERIENCE" "FORMAT-EXPERIENCE"
      ; database-utils
      "DATABASE" "MYSQL-DATABASE"
      "GET-DB-HOST" "SET-DB-HOST" "GET-DB-USER" "SET-DB-USER"
      "GET-DB-USER-PW" "SET-DB-USER-PW" "GET-DB-NAME" "SET-DB-NAME"
      "MAKE-DATABASE-SPEC" "CLEAR-DATABASE"
      "MAKE-DB-STRING"
    ; learning systems
    "GENERIC-LEARNING-SYSTEM" "GENERIC-LEARNING-EXPERIENCE"
    "WEKA" "WEKA-EXPERIENCE" "WEKA-M5PRIME" "WEKA-J48" "WEKA-SVM"
    "LIBSVM" "LIBSVM-EXPERIENCE"
    "SNNS" "SNNS-EXPERIENCE"
    )
  (:import-from "CRAM-UTILITIES"
                "CREATE-GLOBAL-STRUCTURE" "REMOVE-GLOBAL-STRUCTURE" "CLEAR-GLOBAL-STRUCTURE"
                "ADDGV" "ISGV" "GETGV" "SETGV" "REMGV" "PUTGV"))

;(defpackage "ROLL"
;  (:documentation "Robot Learning Language")
;  (:use #:cpl #:roslisp #:utilities)
;;  (:export)
;  (:import-from "UTILITIES"))
