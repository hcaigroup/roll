(in-package :roll)

(defmacro generate-parameter-samples (&key parameters (relation nil rp))
  (multiple-value-bind (rel var-list) (extract-rel-and-vars parameters relation rp)
    (let ( (count-var (gensym "COUNT"))
           (internal-list-vars (mapcar #'(lambda (sym) (cons sym (gensym (symbol-name sym)))) var-list)) )
      (multiple-value-bind (episode-count updated-param-defs) (count-episodes rel parameters)
        `(let*( (,count-var ,episode-count)
                ,@(mapcan
                   #'(lambda (pp) (transform-parameter pp count-var internal-list-vars))
                   (normalize-param-defs updated-param-defs parameters)) )
          (values
            ,(transform-relation rel internal-list-vars)
            ,count-var
            ',var-list))))))

(defmacro with-parameter-samples (parameter-def &body body)
  (destructuring-bind (&key parameters (relation nil rp)) parameter-def
    (multiple-value-bind (rel var-list) (extract-rel-and-vars parameters relation rp)
      (declare (ignore rel))
      (let ( (list-count-var (gensym "II")) )
        `(dolist (,list-count-var (generate-parameter-samples ,@parameter-def))
          (destructuring-bind ,var-list ,list-count-var
            ,@body))))))


;; auxiliaries
; extract-rel-and-vars
(defun extract-rel-and-vars (parameters relation rp)
  (let* ( (rel (if rp
                relation
                (cons :dep (alexandria:flatten (mapcar #'first parameters)))))
          (var-list (alexandria:flatten (cut:rec-remove '(:dep :indep :indep-min :indep-max)
                                                        rel :test #'member))) )
    (values rel var-list)))

; count-episodes
(defun count-episodes (relation parameters)
  (labels ( (find-parameter (pp params &key (test #'eq))
              (when params
                (let ( (firstparam (caar params)) )
                  (if (if (atom firstparam)
                        (funcall test pp firstparam)
                        (find pp firstparam :test test))
                    (first params)
                    (find-parameter pp (rest params))))))
            (count-param-episodes (paramspec)
              (case (first paramspec)
                ( :random (destructuring-bind (&key samples &allow-other-keys) (rest paramspec)
                            samples) )
                ( :cover (destructuring-bind (&key (min 0) (max nil mp) (interval nil ip)) (rest paramspec)
                           (cond ( (null mp)
                                   (error "[GENERATE-PARAMETER-SAMPLES] Maximum value required for :cover sample generation mode") )
                                 ( (null ip)
                                   (error "[GENERATE-PARAMETER-SAMPLES] Interval value required for :cover sample generation mode") )
                                 ( T
                                   (1+ (floor (- max min) (abs interval))) ))) )
                ( :predefined (length (second paramspec)) ))) )
    (cond ( (atom relation)
            (let ( (param-def (find-parameter relation parameters)) )
              (values (count-param-episodes (rest param-def))
                      param-def)) )
          ( T
            (destructuring-bind (rel &rest relargs) relation
              (let*( (arg-counts (mapcar #'(lambda (rr)
                                           (multiple-value-list (count-episodes rr parameters)))
                                         relargs))
                     (countv (apply (case rel
                                      (:dep #'*)
                                      (:indep-min #'min)
                                      ((:indep-max :indep) #'max))
                                    (remove nil (mapcar #'first arg-counts)))) )
                ; complete parameter definitions
                (values
                  countv
                  (mapcar #'(lambda (count-with-param)
                             (if (null (first count-with-param))
                               (append (second count-with-param) `(:samples ,countv))
                               (second count-with-param)))
                          arg-counts)))) ))))

; normalize param-defs
(defun normalize-param-defs (updated-param-defs original-param-defs)
  (labels ( (find-parameter-rec (pp params &key (test #'equal))
              (when params
                (cond ( (funcall test pp (caar params))
                        (first params) )
                      ( T
                        (or
                          (when (listp (caar params)) (find-parameter-rec pp (first params) :test test))
                          (find-parameter-rec pp (rest params) :test test)) )))) )
    (mapcar #'(lambda (pd) (find-parameter-rec (first pd) updated-param-defs))
            original-param-defs)))

(labels ( (get-internal-var-name (sym internal-list-vars)
            (let ( (res (assoc sym internal-list-vars)) )
              (if res
                (rest res)
                (error "[GENERATE-PARAMETER-SAMPLES] variable ~s not specified in problem relation" sym)))) )

  ;; parameter parsing
  (defun transform-parameter (param count-var internal-list-vars)
      (let ( (firstvar (if (atom (first param)) (first param) (caar param)))
             (action (case (second param)
                       ( :random (destructuring-bind (&key (min 0) max samples) (cddr param)
                                   (when (null max)
                                     (error "[GENERATE-PARAMETER-SAMPLES] maximum value for random sample must be provided"))
                                   `(cut:make-random-list
                                     ,(or samples count-var)
                                     :lower ,min
                                     :upper ,max)) )
                       ( :cover (destructuring-bind (&key (min 0) max interval) (cddr param)
                                  `(generate-systematic-list ,min ,max ,interval)) )
                       ( :predefined `',(third param) ))) )
        `((,(get-internal-var-name firstvar internal-list-vars) ,action)
          ,@(when (listp (first param))
            (mapcar #'(lambda (vv) `(,(get-internal-var-name vv internal-list-vars)
                                     ,(get-internal-var-name firstvar internal-list-vars)))
                    (cdar param))))))

  ;; relation transformation
  (defun transform-relation (relation internal-list-vars)
    (labels ( (build-nested-loops (relations result-sym &optional generated-syms)
                (cond ( (null relations)
                        `(push (alexandria:flatten (list ,@(reverse generated-syms))) ,result-sym) )
                      ( T
                        (let ( (newsym (gensym)) )
                          `(dolist (,newsym ,(transform-relation (car relations) internal-list-vars))
                            ,(build-nested-loops (rest relations) result-sym (cons newsym generated-syms)))) ))) )
      (cond ( (atom relation)
              (get-internal-var-name relation internal-list-vars) )
            ( T
              (case (first relation)
                ( :dep
                  (let ( (ressym (gensym)) )
                          `(let ( (,ressym ()) )
                            ,(build-nested-loops (rest relation) ressym)
                            ,ressym)) )
                ( :indep-min
                  `(mapcar
                    #'(lambda (&rest x) (alexandria:flatten (apply #'list x)))
                    ,@(mapcar #'(lambda (xx) (transform-relation xx internal-list-vars)) (rest relation))) )
                ( (:indep :indep-max)
                  `(max-list-combination ,@(mapcar #'(lambda (xx) (transform-relation xx internal-list-vars))
                                                   (rest relation))) )) ))))

)

;; functions needed after macro evaluation
; generate-systematic-list
(defun generate-systematic-list (minimum maximum interval)
  (if (> minimum maximum)
    ()
    (cond  ( (plusp interval)
             (cons minimum (generate-systematic-list (+ minimum interval) maximum interval)) )
           ( (minusp interval)
             (cons maximum (generate-systematic-list minimum (+ maximum interval) interval)) )
           ( T
             (error "[PROBLEM GENERATOR] Interval must not be zero!") ))))

; max-list-combination
(defun max-list-combination (&rest lists)
  (labels ( (lengthen-list (ll min-length &optional (result ll))
              (if (or (>= (length result) min-length)
                      (null ll))
                result
                (lengthen-list ll min-length (append result ll)))) )
    (let ( (min-length (apply #'max (mapcar #'length lists))) )
      (apply #'mapcar #'(lambda (&rest x) (alexandria:flatten (apply #'list x)))
        (mapcar #'(lambda (ll) (lengthen-list ll min-length)) lists)))))
