(in-package :roll)

; general weka learning system
(defclass weka (learning-system)
  ( (arff-file :reader arff-file :initarg :arff-file)
    (weka-output-file :reader weka-output-file :initarg :weka-output-file)
    (delete-weka-output-file :initarg :delete-weka-output-file :initform nil :reader delete-weka-output-file) ))

; initialize-instance: set directories, file names and extensions
(defmethod initialize-instance :after ((ls weka) &key (root-dir (namestring (user-homedir-pathname)))
                                                      (arff-dir root-dir) (weka-dir root-dir)
                                                      (learned-dir root-dir) (data-dir root-dir))
  (let*( (root-path (make-pathname :directory root-dir))
         (data-path (merge-pathnames (make-pathname :directory data-dir) root-path)) )
    (setf (slot-value ls 'arff-file)
      (merge-pathnames
        (make-pathname :directory arff-dir :name (learning-problem-name ls) :type "arff")
        data-path))
    (setf (slot-value ls 'weka-output-file)
      (merge-pathnames
        (make-pathname :directory weka-dir :name (learning-problem-name ls) :type "weka")
        data-path))
    (unless (learned-function-file ls)
      (setf (learned-function-file ls)
        (merge-pathnames
          (make-pathname :directory learned-dir :name (learning-problem-name ls) :type "lisp")
          root-path)))))

; weka using any kind of decision tree
(defclass weka-tree (weka)
  ( (use-unpruned-tree :initarg :use-unpruned-tree :initform nil :accessor use-unpruned-tree)
    (minimum-number-of-instances :initarg :minimum-number-of-instances :initform nil
                                 :accessor minimum-number-of-instances) ))

; weka using M5Prime algorithms (model trees/regression trees)
(defclass weka-m5prime (weka-tree)
  ( (use-unsmoothed-predictions :initarg :use-unsmoothed-predictions :initform nil
                                :accessor use-unsmoothed-predictions)
    (build-regression-tree :initarg :build-regression-tree :initform nil
                           :accessor build-regression-tree) ))

; weka using classical decision tree
(defclass weka-j48 (weka-tree)
  ( (pruning-confidence-threshold :initarg :pruning-confidence-threshold :initform nil
                                  :accessor pruning-confidence-threshold)
    (reduced-error-pruning :initarg :reduced-error-pruning :initform nil
                           :accessor reduced-error-pruning)
    (number-of-folds :initarg :number-of-folds :initform nil :accessor number-of-folds)
    (use-binary-splits-only :initarg :use-binary-splits-only :initform nil
                            :accessor use-binary-splits-only) ))

; weka for support vector machines using the SMO classifier
(defclass weka-svm (weka)
  ( (model-file :reader model-file :initarg :model-file)
    (integration-testing-file :reader integration-testing-file :initarg :integration-testing-file)
    (integration-output-file :reader integration-output-file :initarg :integration-output-file)
    (complexity-constant :initarg :complexity-constant :initform nil :accessor complexity-constant)
    (polynomial-exponent :initarg :polynomial-exponent :initform nil :accessor polynomial-exponent)
    (rbf-gamma :initarg :rbf-gamma :initform nil :accessor rbf-gamma)
    (normalize-or-standardize :initform nil :accessor normalize-or-standardize)
    (normalize-feature-space :initarg :normalize-feature-space :initform nil :accessor normalize-feature-space)
    (use-lower-order-terms :initarg :use-lower-order-terms :initform nil :accessor use-lower-order-terms)
    (use-rbf-kernel :initarg :use-rbf-kernel :initform nil :accessor use-rbf-kernel)
    (kernel-cache-size :initarg :kernel-cache-size :initform nil :accessor kernel-cache-size)
    (tolerance-parameter :initarg :tolerance-parameter :initform nil :accessor tolerance-parameter)
    (epsilon-round-off-error :initarg :epsilon-round-off-error :initform nil :accessor epsilon-round-off-error)
    (fit-logistic-models :initarg :fit-logistic-models :initform nil :accessor fit-logistic-models)
    (folds-for-cross-validation :initarg folds-for-cross-validation :initform nil
                                :accessor folds-for-cross-validation)
    (random-seed-cross-validation :initarg random-seed-cross-validation :initform nil
                                  :accessor random-seed-cross-validation) ))


(defmethod initialize-instance :after ((ls weka-svm) &key (root-dir (namestring (user-homedir-pathname)))
                                                          (data-dir "")
                                                          (model-dir "")
                                                          (integration-dir "")
                                                          normalize
                                                          standardize
                                                          dont-normalize-or-standardize
                                                     &allow-other-keys)
  (let*( (root-path (make-pathname :directory root-dir))
         (data-path (merge-pathnames (make-pathname :directory data-dir) root-path)) )
    (setf (slot-value ls 'model-file)
      (merge-pathnames
        (make-pathname :directory model-dir
                       :name (learning-problem-name ls)
                       :type "model")
        data-path))
    (setf (slot-value ls 'integration-testing-file)
      (merge-pathnames
        (make-pathname :directory integration-dir
                       :name (cut:string-concat (learning-problem-name ls) "-test")
                       :type "arff")
        data-path))
    (setf (slot-value ls 'integration-output-file)
      (merge-pathnames
        (make-pathname :directory integration-dir
                       :name (cut:string-concat (learning-problem-name ls) "-testoutput")
                       :type "res")
        data-path)))
  ; set value for parameter normalize-or-standardize
  (let ( redundant-spec )
    (cond ( normalize
            (setf (normalize-or-standardize ls) 0)
            (setf redundant-spec (or standardize dont-normalize-or-standardize)) )
          ( standardize
            (setf (normalize-or-standardize ls) 1)
            (setf redundant-spec dont-normalize-or-standardize) )
          ( dont-normalize-or-standardize
            (setf (normalize-or-standardize ls) 2) ))
    (when redundant-spec
      (warn "[WEKA-SVM] Redundant specification for support vector machine:~%normalize: ~a~%standardize: ~a~%, don't normalize or standardize: ~a"
      normalize standardize dont-normalize-or-standardize))))

; integration-testing-file/integration-output-file:
; temporary file used for generating test arff files and weka outputs when using the learned function
; (see also integration-weka-svm.lisp)

