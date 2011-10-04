(in-package :roll)

; class definition
(defclass recorder ()
  ( (name :initform "" :initarg :name :reader name)
    (frequency :initform 0.1 :initarg :frequency :accessor frequency)
    (recording-code :initarg :recording-code :accessor recording-code)
    (signature :initarg :signature :accessor signature)
    (data :initarg :data :initform () :accessor data)
    (thread :accessor thread) ))

; generic functions
(defgeneric activate (recorder))
(defgeneric deactivate (recorder))
(defgeneric process-recording (recorder &key output action))
(defgeneric recording-available (recorder))

; start and stop recording process
(defmethod activate ((rec recorder))
  (setf (thread rec)
        (cut:spawn-thread (name rec)
                          #'(lambda () (loop (sleep (frequency rec))
                                             (push (eval (recording-code rec))
                                                   (data rec)))))))

(defmethod deactivate ((rec recorder))
  (cut:kill-thread (thread rec)))


(defmethod process-recording ((rec recorder) &key (output :nothing) (action :keep-all))
  (prog1
    (let ( (ordered-data (reverse (data rec))) )
      (case output
        (:data ordered-data)
        (:signature (signature rec))
        (:data-sig-values (values ordered-data (signature rec)))
        (:data-sig-assoc
          (mapcar #'cons (signature rec) (apply #'mapcar #'list ordered-data)))
        (otherwise)))
    (case action
      (:delete-data
       (setf (data rec) nil))
      (:delete-signature
       (setf (signature rec) nil))
      (:delete-all
       (setf (data rec) nil)
       (setf (signature rec) nil))
      (otherwise))))

(defmethod recording-available ((rec recorder))
  (plusp (length (data rec))))


