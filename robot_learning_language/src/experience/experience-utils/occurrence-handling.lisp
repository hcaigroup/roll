(in-package :roll)

; reduce-occurrences
; not exported -> functionality is accessible by aggregate-occurrences (but is needed in several places in aggregate-occurrences)
(defmacro reduce-occurrences (fun seq &key initial-value)
  (let ( (v1-sym (gensym))
         (v2-sym (gensym)) )
  `(reduce #'(lambda (,v1-sym ,v2-sym) (funcall ,fun  ,v1-sym (first ,v2-sym)))
           ,@(if initial-value
             `(,seq :initial-value ,initial-value)
             `((rest ,seq) :initial-value (caar ,seq))))))

; aggregate-occurrences
(defmacro aggregate-occurrences (fun seq &key (level :all) (keep-occurrence-numbers T))
  (let ( (subseqs-sym (gensym "SUBSEQS"))
         (level-position-sym (gensym "LEVEL-POSITION"))
         (lambda-sym (gensym)) )
    (cond ( (or (eq level :all) (zerop level))
            `(reduce-occurrences ,fun ,seq) )
          ( (numberp level)
            `(let*( (,level-position-sym (- (length (first ,seq)) ,level))
                    (,subseqs-sym (split-occurrence-sequences ,seq ,level-position-sym)) )
              (mapcar
                ,(if keep-occurrence-numbers
                  `#'(lambda (,lambda-sym)
                          (cons (reduce-occurrences ,fun ,lambda-sym)
                                (subseq (first ,lambda-sym) ,level-position-sym)))
                  `#'(lambda (,lambda-sym) (reduce-occurrences ,fun ,lambda-sym)))
                ,subseqs-sym)) )
          ( T
            (error "[EXPERIENCE CONVERSION] Invalid level specification in aggregate occurrences: ~s~%" level) ))))

; map-occurrences
(defmacro map-occurrences (fun &rest args)
  (multiple-value-bind (keep-occurrence-numbers sequences) (if (eq (first args) :keep-occurrence-numbers)
                                                               (values (second args) (cddr args))
                                                               (values T args))
    (let ( (lambda-sym (gensym))
           (lambda-args (gensym "ARGS")) )
      `(apply #'mapcar
              #'(lambda (&rest ,lambda-args)
                (unless (every #'(lambda (,lambda-sym) (equal (rest ,lambda-sym) (cdar ,lambda-args)))
                               (rest ,lambda-args))
                  (warn "[EXPERIENCE CONVERSION] Incompatible occurrence specification in map-occurrences"))
                ,(if keep-occurrence-numbers
                  `(cons (apply ,fun (mapcar #'first ,lambda-args)) (cdar ,lambda-args))
                  `(apply ,fun (mapcar #'first ,lambda-args))))
             (list ,@sequences)))))

; filter-occurrences
(defun filter-occurrences (seq occurrence-spec)
  (remove-if #'(lambda (x) (not (alexandria:ends-with-subseq occurrence-spec x))) seq))


;; auxiliary function
(defun split-occurrence-sequences (seq from-nth-occ-no &optional pattern split-sequences current-sequence)
  (cond ( (null seq)
          (reverse (if current-sequence
                     (cons (reverse current-sequence) split-sequences)
                     split-sequences)) )
        ( (null pattern)
          (split-occurrence-sequences seq from-nth-occ-no (subseq (first seq) from-nth-occ-no) split-sequences nil) )
        ( (alexandria:ends-with-subseq pattern (first seq))
          (split-occurrence-sequences (rest seq) from-nth-occ-no pattern split-sequences
            (cons (first seq) current-sequence)) )
        ( T
          (split-occurrence-sequences seq from-nth-occ-no (subseq (first seq) from-nth-occ-no)
            (cons (reverse current-sequence) split-sequences) nil) )))
