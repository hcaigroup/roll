(in-package :roll)

(defun make-learning-conversion-code (conversion-spec replace-list)
  (labels ( (replace-variables-in-conversion-spec (spec &optional (replist replace-list))
              (cond ( (null replist)
                      spec )
                    ( T
                      (subst (cadar replist) (caar replist)
                             (replace-variables-in-conversion-spec spec (rest replist))
                             :test #'equal) ))) )

    (case (first conversion-spec)
      ( with-cross-product
        (warn "[SNNS-INTEGRATION] with-cross-product not yet supported for input conversion")
        (make-learning-conversion-code
          `(with-binding ,(caadr conversion-spec) ,(third conversion-spec))
          replace-list) )
      ( with-filter
        (make-learning-conversion-code
          `(with-binding ,(caadr conversion-spec) ,(third conversion-spec))
          replace-list) )
      ( with-binding
        (append
          (replace-variables-in-conversion-spec (second conversion-spec))
          (make-learning-conversion-code (third conversion-spec) replace-list)) )
      ( T
        (replace-variables-in-conversion-spec conversion-spec) ))))
