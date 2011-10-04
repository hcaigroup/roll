(in-package :roll)

;; parse-weka-output

(defgeneric skip-weka-head (ls weka-input))
(defgeneric parse-branch-operation (ls str))
(defgeneric parse-branch (ls branch-string))

; m5prime
(defmethod parse-weka-output ((ls weka-tree) attributes)
  (declare (ignore attributes))
  (with-open-file (weka-input (weka-output-file ls))
    (labels ( ;; parse-weka-tree
              (parse-weka-tree ()
                (labels ( ; count-bars
                          (count-bars (str &optional (counter 0))
                            (cond ( (peek-char #\| str nil nil)
                                    (read-char str)
                                    (count-bars str (1+ counter)) )
                                  ( T
                                    counter ))) )

                  (unless (char= (peek-char nil weka-input) #\Newline)
                    (let ( (next-line (read-line weka-input)) )
                      (cond ( (and (>= (length next-line) 2)
                                   (string= (subseq next-line 0 2) "LM"))
                              (destructuring-bind (rule-name &rest other-info) (cut:split-one next-line #\Space)
                                (declare (ignore other-info))
                                (list (intern rule-name))) )
                            ( T
                              (let ( (bar-count (with-input-from-string (str next-line) (count-bars str)))
                                     (line-content (string-trim "| " next-line)) )
                                (multiple-value-bind (branch-condition branch-content)
                                                     (parse-branch ls line-content)
                                  (cons
                                    (cons bar-count
                                      (cons branch-condition
                                        (when branch-content (list branch-content))))
                                    (parse-weka-tree)))) )))))) )

      (skip-weka-head ls weka-input)
      ; integrate-learned-function
      (let ( (weka-tree (parse-weka-tree)) )
        (cond ( (typep ls 'weka-m5prime)
                (list->tree-m5prime weka-tree (parse-branch-functions weka-input)) )
              ( (typep ls 'weka-j48)
                (list->tree-j48 weka-tree) ))))))

;skip-weka-head(ls weka-j48)
(defmethod skip-weka-head ((ls weka-m5prime) weka-input)
  (let ( (next-line (read-line weka-input)) )
    (cond ( (and (>= (length next-line) 2)
                 (string= (subseq next-line 0 2) "M5"))
            (read-line weka-input)
            (peek-char t weka-input) )
          ( T
            (skip-weka-head ls weka-input) ))))

(defmethod skip-weka-head ((ls weka-j48) weka-input)
  (let ( (next-line (read-line weka-input)) )
    (cond ( (and (>= (length next-line) 2)
                 (string= (subseq next-line 0 3) "J48"))
            (read-line weka-input)
            (peek-char t weka-input) )
        ( T
          (skip-weka-head ls weka-input) ))))


;; read-symbolic-attribute-spec (auxiliary for parse-branch and parse-branch-functions)
; parse tree branch arguments of the form "attr[=val1[,val2...,valn]]"
; for attributes with numeric values, which are usualy of the form "attr", a simple (read input-stream) would do
(defun read-symbolic-attribute-spec (input-stream &optional parameter-symbol collected-values)
  (case (peek-char nil input-stream nil :eof-found)
    ( #\Space
      (multiple-value-bind (split-res split-no) (cut:split-one (cut:->string parameter-symbol) #\=)
        (if (zerop split-no)
          (values parameter-symbol collected-values nil)
          (values (cut:string->symbol (first split-res))
                  (cons (cut:string->symbol (second split-res)) collected-values)
                  T))) )
    ( #\,
      (read-char input-stream)
      (read-symbolic-attribute-spec input-stream
                                    parameter-symbol
                                    (cons (read-preserving-whitespace input-stream) collected-values)) )
    ( :eof-found
      nil )
    ( otherwise
      (read-symbolic-attribute-spec input-stream
                                    (read-preserving-whitespace input-stream)
                                    collected-values) )))

; parse-branch-operation for m5prime and j48 trees
(defmethod parse-branch-operation ((ls weka-tree) str)
  (multiple-value-bind (sym sym-args uses-symbolic-values-p) (read-symbolic-attribute-spec str)
    (let ( (op (read str))
           (nr (read str)) )
      (when (typep ls 'weka-m5prime)
        (unless (eq (peek-char nil str nil nil) #\:)
          (error "[WEKA-INTEGRATION] expected ':' when parsing weka-file"))
        (read-char str))
      (cond ( uses-symbolic-values-p
              (let ( (or-clause (if (null (rest sym-args))
                                    `(eq ,sym ',(first sym-args))
                                    (cons 'or (mapcar #'(lambda (arg) `(eq ,sym ',arg)) sym-args)))) )
                (cond ( (= nr 0.5)
                        (if (eq op '>)
                          or-clause
                          `(not ,or-clause)) )
                      ( T
                        (error "[WEKA-INTEGRATION] Limit for nominal value should be 0.5, but is ~a." nr) ))) )
            ( (numberp nr)
              `(,op ,sym ,nr) )
            ( (eq op '=)
              `(eq ,sym ',nr) )
            ( T
              (warn "[WEKA-INTEGRATION] unknown syntax of WEKA tree")
              `(,op ,sym ,nr) )))))


; parse-branch
(defmethod parse-branch ((ls weka-m5prime) branch-string)
  (with-input-from-string (str branch-string)
    (values
      (parse-branch-operation ls str)
      (read str nil nil))))

(defmethod parse-branch ((ls weka-j48) branch-string)
  (destructuring-bind (condition-string &optional value-string) (cut:split-one branch-string #\:)
    (values
      (if (string= condition-string "")
        'T
        (with-input-from-string (str condition-string)
          (parse-branch-operation ls str)))
      (when value-string
        (with-input-from-string (str value-string)
          (let ( (branch-result (read str nil nil)) )
            (if (symbolp branch-result)
              `',branch-result
              branch-result)))))))

;; parse-branch-functions
(defun parse-branch-functions (weka-input &optional (hashed-res (make-hash-table)) (formula-count 0))
  (labels ( ; read-linear-formula
            (read-linear-formula (str &optional terms)
              (cond ( (char= (peek-char nil str nil nil) #\Tab)
                      (let ( (new-term (read-term (read-line str))) )
                        (read-linear-formula str (cons new-term terms))) )
                    ( T
                      (cons '+ terms) )))
            ; read-term
            (read-term (input-string)
              (with-input-from-string (str input-string)
                (peek-char t str)
                (let*( (op (if (numberp (peek-char t str))
                               #\+
                               (read-char str)))
                       (arg1 (read str))
                       (op-times (read str nil nil)) )
                  (multiple-value-bind (sym sym-args uses-symbolic-values-p) (read-symbolic-attribute-spec str)
                    (let*( (arg2 (cond ( uses-symbolic-values-p
                                         (let ( (test-args (mapcar #'(lambda (val) `(eq ,sym ',val))
                                                                   sym-args)) )
                                           `(cut:bool->float ,(if (null (rest sym-args))
                                                                 (first test-args)
                                                                 `(or ,@test-args)))) )
                                       ( T
                                         sym )))
                           (simple-term (if (and op-times arg2)
                                            (list op-times arg1 arg2)
                                            arg1)) )
                      (if (eq op #\-)
                        (list '- simple-term)
                        simple-term)))))) )
    (read-line weka-input)
    (let ( (next-line (read-line weka-input)) )
      (destructuring-bind (comment nr) (cut:split-one next-line #\Space :from-end T)
        (cond ( (string= (subseq comment 0 2) "LM")
                (let*( (lm-sym (alexandria:format-symbol *package* "LM~a" nr))
                       (res-var (string-right-trim "= " (read-line weka-input)))
                       (formula (read-linear-formula weka-input)) )
                  (declare (ignore res-var))
                  (setf (gethash lm-sym hashed-res) formula)
                  (parse-branch-functions weka-input hashed-res (1+ formula-count))) )
              ( (string-equal comment "Number of Rules :")
                (let ( (weka-rule-count (read-from-string nr)) )
                  (unless (= formula-count weka-rule-count)
                    (warn "[WEKA-INTEGRATION] unexpected number of rules (~a), exspected ~a"
                    weka-rule-count
                    formula-count)))
                hashed-res ))) )))


;; list->tree
; transform list of form ((0 A) (1 B X) (1 C) (2 D Y) (2 E Z) (0 F))
; to (COND (A (COND (B X) (C (COND (D Y) (E Z))))) (F NIL))
; thereby replacing X, Y and Z with values specified in hash table
(defun list->tree-m5prime (ll hashed-functions &optional (current-depth -1))
  (unless (endp ll)
    (destructuring-bind (first-ll &rest rest-ll) ll
      (cond ( (symbolp first-ll)
              (gethash first-ll hashed-functions) )
            ( (= (first first-ll) current-depth)
              (cond ( (third first-ll)
                      (cons
                        (list (second first-ll)
                              (gethash (third first-ll) hashed-functions))
                        (list->tree-m5prime rest-ll hashed-functions current-depth)) )
                    ( T
                      (multiple-value-bind (subtree-list siblings-list)
                                           (get-subtree-from-list rest-ll current-depth)
                        (cons (list (second first-ll) (list->tree-m5prime subtree-list hashed-functions current-depth))
                              (list->tree-m5prime siblings-list hashed-functions current-depth))) )) )
            ( (> (first first-ll) current-depth)
                `(cond ,@(list->tree-m5prime ll hashed-functions (1+ current-depth))) )))))

(defun list->tree-j48 (ll &optional (current-depth -1))
  (unless (endp ll)
    (destructuring-bind (first-ll &rest rest-ll) ll
      (cond ( (= (first first-ll) current-depth)
              (cond ( (third first-ll)
                      (cons
                        (list (second first-ll)
                              (third first-ll))
                        (list->tree-j48 rest-ll current-depth)) )
                    ( T
                      (multiple-value-bind (subtree-list siblings-list)
                                           (get-subtree-from-list rest-ll current-depth)
                        (cons (list (second first-ll) (list->tree-j48 subtree-list current-depth))
                              (list->tree-j48 siblings-list current-depth))) )) )
            ( (> (first first-ll) current-depth)
              `(cond ,@(list->tree-j48 ll (1+ current-depth))) )))))

; get-subtree-from-list
(defun get-subtree-from-list (ll nr &optional res)
  (cond ( (endp ll)
          (reverse res) )
        ( (<= (caar ll) nr)
          (values (reverse res) ll) )
        ( T
          (get-subtree-from-list (rest ll) nr (cons (first ll) res)) )))

