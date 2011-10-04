(in-package :roll)

(defmethod cpl-impl::execute :before ((task cpl-impl::abstract-task))
  (notify-new-task task)
  (assert (slot-value task 'cpl-impl::thread-fun) ()
          "Tried to execute task ~S which has no thread-fun." task)
  (assert (not (slot-value task 'cpl-impl::thread)) ()
          "Task ~S is already running a thread." task))


(let ( (requested-tasks (make-hash-table :test 'equal)) )

  (defun get-requested-task (task-prefix request-id)
    (let ( (hash-entry (gethash (cut:->string task-prefix) requested-tasks)) )
      (when hash-entry
        (if (eq request-id :all)
          hash-entry
          (cdr (assoc request-id hash-entry))))))

  (defun add-requested-task (task-prefix task-register-fluent request-id)
    (remove-requested-task task-prefix request-id)
    (push (cons request-id task-register-fluent) (gethash (cut:->string task-prefix) requested-tasks)))

  (defun remove-requested-task (task-prefix request-id)
    (let*( (task-prefix-string (cut:->string task-prefix))
           (hash-entry (gethash task-prefix-string requested-tasks)) )
      (if (null hash-entry)
        (remhash task-prefix-string requested-tasks)
        (setf (gethash task-prefix-string requested-tasks)
              (remove request-id hash-entry :key #'car)))))

  (defun register-task-request (task-prefix request-id)
    (let ( (task-register-fluent (make-fluent :name task-prefix :value nil)) )
      (add-requested-task task-prefix task-register-fluent request-id)
      task-register-fluent))

  (defun get-task-name-prefix (task-name)
    (destructuring-bind (name-prefix &optional name-rest) (cut:split-one task-name #\- :from-end T)
      (cond ( (null name-rest)
              name-prefix )
            ( (numberp (read-from-string (string-left-trim '(#\#) name-rest)))
              (get-task-name-prefix (string-trim '(#\[ #\]) name-prefix)) )
            ( T
              task-name ))))

  (defun notify-new-task (task)
    (let*( (task-prefix (get-task-name-prefix (cut:->string (cpl-impl::task-name task))))
           (task-request-fluents (get-requested-task task-prefix :all)) )
      (dolist (ii task-request-fluents)
        (setf (value (cdr ii)) task))))

)
