(in-package :roll)

(defgeneric make-snns-function-call (lp in-out-let in-var-conversion
                                    fun-call out-var-conversion))

;; integrate-learned-function
(defmethod integrate-learned-function ((ls snns) (lp learning-problem))
  (let ( (in-sig (first (learning-signature ls)))
         (out-sig (second (learning-signature ls))) )
    ; load library
    (uffi:load-foreign-library (library-file ls))
    ; write function to file
    (with-open-file (str (learned-function-file ls) :direction :output :if-exists :supersede)
      (format str "(in-package ~s)~2%" (package-name (learned-function-package ls)))
      (ensure-directories-exist (learned-function-file ls))
      ; foreign function definition
      (format str "~s~2%" `(uffi:def-function
                              ,(substitute #\_ #\- (learning-problem-name ls))
                              ((in (cl-user::* :float)) (out (cl-user::* :float)) (init :int))
                              :returning :int))
      ; foreign function call
      (format str "~s~2%" (make-snns-function-call
                            lp
                            `( (in (uffi:allocate-foreign-object :float ,(length in-sig)))
                               (out (uffi:allocate-foreign-object :float ,(length out-sig))) )
                            (let ( (in-vars ()) )
                              (dotimes (ii (length in-sig) in-vars)
                                (push
                                  `(setf (uffi:deref-array in '(cl-user::* :float) ,ii) (coerce ,(nth ii in-sig) 'single-float))
                                  in-vars)))
                            `(,(cut:string->symbol (learning-problem-name ls)) in out 0)
                            `(let ,(let ( (out-vars ()) )
                                    (dotimes (ii (length out-sig) out-vars)
                                      (push
                                        (list (nth ii out-sig)
                                              `(uffi:deref-array out '(cl-user::* :float) ,ii))
                                        out-vars)))
                              ,@(output-conversion lp)))))))

;;; special treatment of different learning problem classes

;; method-learning-problem
(defmethod make-snns-function-call ((lp method-learning-problem) in-out-let in-var-conversion
                                    fun-call out-var-conversion)
    (let ( (lambda-list (mapcar #'(lambda (var)
                                    (let ( (spec (find var (specializers lp) :key #'first)) )
                                      (if spec
                                        spec
                                        var)))
                                (clos:generic-function-lambda-list (symbol-function (generic-fun lp))))) )
      ; code to be returned
      `(let ,in-out-let
        (defmethod ,(generic-fun lp) ,lambda-list
          (let* ,(make-learning-conversion-code (input-conversion lp) nil)
            ,@in-var-conversion
            ,fun-call
            ,out-var-conversion)))))

;; general-function-learning-problem
(defmethod make-snns-function-call ((lp general-function-learning-problem) in-out-let in-var-conversion
                                    fun-call out-var-conversion)
  `(let ,in-out-let
    (defun ,(funname lp) ,(args lp)
      (let*,(make-learning-conversion-code (input-conversion lp) ())
        ,@in-var-conversion
        ,fun-call
        ,out-var-conversion))))



