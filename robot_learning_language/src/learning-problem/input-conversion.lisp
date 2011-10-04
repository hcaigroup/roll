(in-package :roll)

;;; expand-input-conversion (main function)
;; Purpose:
; calculate from all abstractions between the given abstract and some raw experience
; a completely expanded abstraction specification to be used for calling the learned function
;; Strategy:
; 1. find trace of experiences leading from learning experience to specified experience
;    taking into account the restrictions given by conversion-spec
; 2. combine automata recursively from abstract to raw experience
; 3. filter relevant parts for learning

(defun expand-input-conversion (abstract-experience conversion-spec)
  (if (eq (first conversion-spec) :generate)
    (destructuring-bind (trace-restrictions replacement-specs) (parse-conversion-spec (rest conversion-spec))
      (let ( (exp-trace (first (find-experience-traces (getgv :experience abstract-experience)
                                                       trace-restrictions))) )
        exp-trace
        (if (null exp-trace)
          (error "[INPUT-CONVERSION] no valid trace for experience ~a could be found with restrictions ~{~a~^, ~}"
                 abstract-experience
                 trace-restrictions)
          (prune-combined-experience (combine-experiences replacement-specs exp-trace)))))
      conversion-spec))

;; parsing
; parse-conversion-spec
; find restrictions on experience traces and convert substitution specifications to a more general form
(defun parse-conversion-spec (conversion-spec)
  (labels ( (parse-substitution-specs (key1 arg1 key2 arg2 &rest other-args)
              (multiple-value-bind (replacements var-binds)
                                   (when other-args (apply #'parse-substitution-specs other-args))
                (ccase key1
                  ( :replace
                    (if (eq key2 :by)
                      (values (cons (list arg1 arg2) replacements)
                              var-binds)
                      (warn "[INPUT-CONVERSION] Keywords :REPLACE and ~s do not fit" key2)) )
                  ( :set-var
                    (if (eq key2 :to)
                      (values replacements
                              (cons (list arg1 arg2) var-binds))
                      (warn "[INPUT-CONVERSION] Keywords :SET-VAR and ~s do not fit" key2)) )))) )
    (reduce #'(lambda (llist genspec)
                (destructuring-bind (trace-restrictions substitution-specs) llist
                  (multiple-value-bind (experience parent-experience subst-specs)
                                       (ecase (first genspec)
                                         ( :in-experience
                                           (values (second genspec)
                                                   nil
                                                   (cddr genspec)) )
                                         ( :in-conversion
                                           (values (third genspec)
                                                   (second genspec)
                                                   (cdddr genspec)) ))
                    (multiple-value-bind (repl var-bind) (apply #'parse-substitution-specs subst-specs)
                      (list
                        (if parent-experience
                          (adjoin parent-experience (adjoin experience trace-restrictions))
                          (adjoin experience trace-restrictions))
                        (multiple-value-bind (available-experience-data other-subst-specs)
                                             (cut:find-and-remove experience substitution-specs :key #'first)
                          (cons
                            (if available-experience-data
                              (list (first available-experience-data)
                                    (union repl (second available-experience-data) :test #'equal)
                                    (union var-bind (third available-experience-data) :test #'equal))
                              (list experience repl var-bind))
                            other-subst-specs)))))))
            conversion-spec
            :initial-value '(() ()))))


; find-experience-traces
; searches on experience objects (necessary for finding children)
; if it returns null, no valid trace could be found
; if several traces are found, any of those can be used
(defun find-experience-traces (experience required-waypoints)
  (cond ( (typep experience 'raw-experience)
          (when (null (remove (name experience) required-waypoints))
            (list (list experience))) )
        ( T
          (reduce
            #'(lambda (ll ex)
               (let ( (parent-traces (find-experience-traces ex
                                                             (remove (name experience)
                                                             required-waypoints))) )
                 (append
                   (mapcar (alexandria:curry #'cons experience) parent-traces)
                   ll)))
            (parent-experiences experience)
            :initial-value ()) )))


;; combine experiences
; combine-experiences
; combines experience objects of experience trace, thereby substituting variables as specified by the user
(defun combine-experiences (substitution-specs exp-trace &optional generated-code)
  (cond ( (null exp-trace)
          nil )
        ( (= (length exp-trace) 1)
          (let ( (substituted-raw-exp-code (make-substitutions
                                             (rest (find (name (first exp-trace)) substitution-specs
                                                         :key #'first))
                                             (specification-code (first exp-trace)))) )
            (if generated-code
              (combine-experience-codes generated-code substituted-raw-exp-code)
              substituted-raw-exp-code)) )
        ( T
          (let*( (abstract (name (first exp-trace)))
                 (raw (name (second exp-trace)))
                 (substituted-abstract-code (make-substitutions
                                              (rest (find abstract substitution-specs :key #'first))
                                              (get-conversion-spec raw abstract))) )
            (combine-experiences
              substitution-specs
              (rest exp-trace)
              (combine-experience-codes generated-code substituted-abstract-code))) )))

; make-substitutions
; makes replacement as given by the user with :replace ... :by ... and :set-var ... :to ...
(defun make-substitutions (substitution-spec conversion-code)
  (if (null substitution-spec)
    conversion-code
    (destructuring-bind (replace-specs var-binds) substitution-spec
      (rebind-vars
        (reduce #'(lambda (ll repl) (subst (second repl) (first repl) ll :test #'equal))
                replace-specs
                :initial-value conversion-code)
        var-binds))))




; rebind-vars
; make replacements given by :set-var ... :to ...
(defun rebind-vars (autspec variable-specs)
  (labels ( (rebind-list (var-list var-specs event autname)
              (reduce #'(lambda (results var)
                          (destructuring-bind (var-defs repl-specs) results
                            (list
                              (cons
                                (replace-var-in-definition
                                  var
                                  (if (or event autname)
                                      (filter-replace-specs repl-specs event autname)
                                      repl-specs))
                                var-defs)
                              repl-specs)))
                var-list
                :initial-value (list nil var-specs))) )
    (if (or (null variable-specs) (null autspec) (atom autspec))
      autspec
      (case (first autspec)
        ( (with-cross-product with-filter)
          (list (first autspec) (second autspec) (rebind-vars (third autspec) variable-specs)) )
        ( with-binding
          (destructuring-bind (new-var-list new-var-specs)
                              (rebind-list (second autspec) variable-specs nil nil)
            (list 'with-binding
                  (reverse new-var-list)
                  (rebind-vars (third autspec) new-var-specs nil nil))) )
        ( T
          (destructuring-bind (autname &key begin end interval children &allow-other-keys) autspec
            (destructuring-bind (new-begin begin-var-spec-rest)
                                (rebind-list begin variable-specs :begin autname)
              (destructuring-bind (new-end end-var-spec-rest)
                                  (rebind-list end begin-var-spec-rest :end autname)
                (destructuring-bind (new-interval interval-var-spec-rest)
                                    (rebind-list interval end-var-spec-rest :interval autname)
                  (declare (ignore interval-var-spec-rest))
                  (cons
                    (first autspec)
                    (append
                      (when new-begin (list :begin new-begin))
                      (when new-end (list :end new-end))
                      (when new-interval (list :interval new-interval))
                      (when children (list :children
                                           (mapcar #'(lambda (child) (rebind-vars child variable-specs))
                                                   children))))))))) )))))

(defun filter-replace-specs (repl-specs event autname)
  (remove-if #'null
            (mapcar #'(lambda (r-spec)
                        (let ( (rs (first r-spec)) )
                          (cond ( (symbolp rs)
                                  (cons rs (rest r-spec)) )
                                ( (or (and (cddr rs)
                                           (eq event (second rs))
                                           (eq autname (third rs)))
                                      (eq event (second rs)))
                                  (cons (first rs) (rest r-spec)) )
                                ( T
                                  nil ))))
                    repl-specs)))


(defun replace-var-in-definition (vardef repl-specs)
  (let ( (new-def (find (first vardef) repl-specs :key #'first)) )
    (if new-def
      (list (first vardef) (second new-def))
      vardef)))

; combine-experience-codes
; cross-products are not included, their variable definitions are replaced
(defun combine-experience-codes (abstract raw)
  (labels ( ; get-substitution-pairs
            ; generates possible specifications to be replaced (this is not the most efficient way!)
            (get-substitution-pairs (conversion-spec)
              (labels ( (make-automaton-spec (aname slot)
                          (if (or (null aname) (keywordp aname))
                            slot
                            (list slot aname))) )
                (if (find (first conversion-spec) '(with-cross-product with-filter with-binding))
                  (get-substitution-pairs (third conversion-spec))
                  (destructuring-bind (autname &key begin end interval children &allow-other-keys)
                                      conversion-spec
                    (append
                      (mapcan #'(lambda (listname keyname)
                                  (mapcar #'(lambda (x)
                                             (list (list :var (first x) (make-automaton-spec autname keyname))
                                                   (second x)))
                                          listname))
                              (list begin end interval)
                              '(:begin :end :interval))
                      (mapcan #'get-substitution-pairs children)))))) )
    (case (first raw)
      ( with-cross-product
        (combine-experience-codes
          abstract
          (reduce #'(lambda (ll sub) (subst (second sub) (first sub) ll))
                  (caadr raw)
                  :initial-value (third raw))) )
      ( with-filter
        (combine-experience-codes abstract (third raw)) )
      ( with-binding
        (list (first raw)
              (second raw)
              (combine-experience-codes abstract (third raw))) )
      ( T
        (if (null abstract)
          raw
          (reduce #'(lambda (ll rp) (subst (second rp) (first rp) ll :test #'match-automaton-var-descriptions))
                  (get-substitution-pairs raw)
                  :initial-value abstract)) ))))


; match-automaton-var-descriptions
(defun match-automaton-var-descriptions (descr1 descr2)
  (labels ( (automaton-matches (a b)
              (or (equal a b)
                  (and (atom a)
                       (listp b)
                       (eq a (first b))
                       (keywordp (second b)))
                  (and (atom b)
                       (listp a)
                       (eq b (first a))
                       (keywordp (second a)))
                  (and (listp a) (listp b)
                       (eq (first a) (first b))
                       (eq (second a) (second b))))) )
    (or (equal descr1 descr2)
        (and (listp descr1)
             (listp descr2)
             (cut:compare descr1 descr2 :key #'first)
             (cut:compare descr1 descr2 :key #'second)
             (cut:compare descr1 descr2 :key #'cdddr :test #'equal)
             (automaton-matches (third descr1) (third descr2))))))

;; filter relevant parts for learning
; prune-combined-experience
(defun prune-combined-experience (experience)
  (case (first experience)
    ( (with-cross-product with-filter)
      (prune-combined-experience (third experience)) )
    ( with-binding
      (let*( (pruned-sub-experience (prune-combined-experience (third experience)))
             (filtered-new-args (remove-if (lambda (elem)
                                  (not (cut:find-atom-rec-if #'(lambda (x) (eq elem x)) pruned-sub-experience)))
                                  (second experience)
                                  :key #'(lambda (x) (if (listp x) (first x) x)))) )
        (if (null filtered-new-args)
          pruned-sub-experience
          (list (first experience)
                filtered-new-args
                pruned-sub-experience))) )
    ( T
      (destructuring-bind (&key begin &allow-other-keys) (rest experience)
        begin) )))

