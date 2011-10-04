(in-package :roll)

;;; -- GENERIC LEARNING SYSTEM ---------------------------------------------------------------------- ;;;
; Provides the means to learn with an arbitrary function, should be a simple one like max, avg, ...     ;
; The generic-learning-experience must always contain an anonymous automaton (e.g. :automaton)!         ;
; This learning system currently only supports learning of functions and methods, no models or routines ;
; When specifying a learning problem, the learned function must be provided as a lisp command           ;
;   for several operations use let or progn.                                                            ;
;;; ------------------------------------------------------------------------------------------------- ;;;

;;; learning system class
(defclass generic-learning-system (learning-system)
  ( (learning-function :initarg :learning-function
                       :initform #'(lambda (begin end) (declare (ignore begin end)) nil)
                       :reader learning-function)
    (learned-function-code :accessor learned-function-code) ))


; initialize-instance: set directories, file names and extensions
(defmethod initialize-instance :after ((ls generic-learning-system)
                                       &key (output-dir (namestring (user-homedir-pathname))))
  (unless (learned-function-file ls)
    (setf (learned-function-file ls)
          (make-pathname :directory output-dir :name (learning-problem-name ls) :type "lisp"))))






