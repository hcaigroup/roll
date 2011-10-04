(in-package :roll)

(defmethod integrate-learned-function ((ls generic-learning-system) (lp learning-problem))
  (ensure-directories-exist (learned-function-file ls))
  (with-open-file (str (learned-function-file ls) :direction :output :if-exists :supersede)
    (format str "(in-package ~s)~2%" (package-name (learned-function-package ls)))
    (format str "~s"
      (make-learned-function-call
        lp
        `(multiple-value-bind ,(second (learning-signature ls)) ,(learned-function-code ls)
          ,@(output-conversion lp))))))

