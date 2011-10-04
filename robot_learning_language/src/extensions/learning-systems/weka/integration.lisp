(in-package :roll)

(defgeneric parse-weka-output (weka-ls attributes))

;; integrate learned function
(defmethod integrate-learned-function ((ls weka) (lp learning-problem))
  (with-open-file (str (learned-function-file ls) :direction :output :if-exists :supersede)
    (format str "(in-package ~s)~2%" (package-name (learned-function-package ls)))
    (format str "~s"
      (make-learned-function-call
        lp
        `(let ( (,(first (second (learning-signature ls)))
                 ,(parse-weka-output ls (attribute-types (experience lp)))) )
          ,@(output-conversion lp)))))
  (when (delete-weka-output-file ls)
    ;(ut:with-real-streams ;; was only necessary for Allegro?
      (port:run-prog "rm"
        :args (list (namestring (weka-output-file ls))))))

