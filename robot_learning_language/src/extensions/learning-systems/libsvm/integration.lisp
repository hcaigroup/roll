(in-package :roll)

(defmethod integrate-learned-function ((ls libsvm) (lp learning-problem))

  (with-open-file (str (learned-function-file ls) :direction :output :if-exists :supersede)
    (let ( (model-var (gentemp "MODEL")) )
      (format str "(in-package ~s)~2%" (package-name (learned-function-package ls)))
      (format str "(let ( (~s (cl-libsvm:load-model ~s)) )~2%"
        model-var
        (namestring (model-file ls)))
      (format str "~s"
        (make-learned-function-call
          lp
          `(let ( (,(caadr (learning-signature ls))
                    (cl-libsvm:predict ,model-var (vector ,@(let ( (counter 0)
                                                                   (vals ()) )
                                                             (dolist (var (first (learning-signature ls))
                                                                      (reverse vals))
                                                               (incf counter)
                                                               (push `(cons ,counter ,var) vals)))))) )
            ,@(output-conversion lp)))))
      (format str ")")) ; closing parenthesis of let
)
