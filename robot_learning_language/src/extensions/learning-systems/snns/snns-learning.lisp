(in-package :roll)

;; do-learning
(defmethod do-learning ((ls snns) (experience snns-experience))
  (ensure-directories-exist (pattern-file ls))
  (ensure-directories-exist (network-file ls))
  (ensure-directories-exist (batchman-file ls))
  (ensure-directories-exist (c-source-file ls))
  (ensure-directories-exist (library-file ls))
  ; finish pattern file
  (let ( (input-sig (first (learning-signature ls)))
         (output-sig (second (learning-signature ls))) )
    (port:run-prog "/bin/bash"
      :args (list
               "-c"
               (format nil "(/usr/bin/mkhead ~a ~a ~a;echo \"# pattern format: ~{~a ~}-> ~{~a~^ ~}\";cat ~a)"
                       (experience-count experience)
                       (length input-sig)
                       (length output-sig)
                       input-sig
                       output-sig
                       (namestring (pattern-tmp-file experience))))
      :output (pattern-file ls) :if-output-exists :supersede))
  (port:run-prog "/usr/bin/rm"
                 :args (list (namestring (pattern-tmp-file experience))))
  (setf (experience-count experience) 0)
  ; write snns files
  (write-snns-net ls)
  (write-batchman-file ls)
  ; run snns
  (port:run-prog "/usr/bin/batchman"
     :args (list
             "-f"
             (namestring (batchman-file ls))
             "-l"
             (namestring (merge-pathnames (make-pathname :type "log")
                                          (batchman-file ls))))
     :output *standard-output*)
  ; create C file
  (translate-snns-output ls))

; write-snns-net
(defun write-snns-net (snns-ls)
  (format t "write net~%")
  (labels ( (write-connections (layers)
              (if (= layers 1)
                ""
                (format nil " -l ~a + ~a + ~a"
                  (1- layers)
                  layers
                  (write-connections (1- layers))))) )
    (let*( (net-funs (list (unit-activation-fun snns-ls) (unit-output-fun snns-ls)))
           (generic-format-string
             (format nil
               "/usr/bin/ff_bignet -p 1 ~~a ~{~a ~}INPUT ~~{-p 1 ~~a ~{~a ~}HIDDEN ~~} -p 1 ~~a ~{~a ~}OUTPUT"
               net-funs
               net-funs
               net-funs)) )
      (port:run-prog "/bin/bash"
        :args (list
                "-c"
                (cut:string-concat
                  (format nil generic-format-string
                    (length (first (learning-signature snns-ls)))
                    (hidden-layers snns-ls)
                    (length (second (learning-signature snns-ls))))
                  (write-connections (+ (length (hidden-layers snns-ls)) 2))
                  (namestring (network-file snns-ls))))
        :output *standard-output*))))

(let ( (trained-net-file "") )

  ; write-batchman-file
  (defun write-batchman-file (snns-ls)
    (setf trained-net-file
      (merge-pathnames
        (make-pathname :name (cut:string-concat (pathname-name (network-file snns-ls)) ".trained"))
        (network-file snns-ls)))
    (with-open-file (stream (batchman-file snns-ls)
                            :direction :output :if-exists :supersede)
      (format stream "loadNet (\"~a\")~%"
        (namestring (network-file snns-ls)))
      (format stream "loadPattern (\"~a\")~%"
        (namestring (pattern-file snns-ls)))
      (when (net-initialization-fun snns-ls)
        (format stream "~%setInitFunc(~{~s~^, ~})" (alexandria:ensure-list (net-initialization-fun snns-ls))))
      (when (net-learning-fun snns-ls)
        (format stream "~%setLearnFunc(~{~s~^, ~})~%" (alexandria:ensure-list (net-learning-fun snns-ls))))
      (format stream "~%initNet()~%")
      (format stream "for ii = 1 to ~a do~%  trainNet ()~%endfor~2%"
        (cycle-bias snns-ls))
      (format stream "saveNet(\"~a\")~%" trained-net-file)
      (format stream "saveResult(~s,1,PAT,FALSE,TRUE,\"create\")" (namestring (result-file snns-ls)))))

  ; translate-snns-output
  (defun translate-snns-output (snns-ls)
    (let ( (cur-dir (port:default-directory)) )
      (sb-posix:chdir (directory-namestring (c-source-file snns-ls)))
      ; run snns2c (changed to current dir, because snns2c has problems with long paths)
      (port:run-prog "/usr/bin/snns2c"
        :args (list (namestring trained-net-file)
                    (file-namestring (c-source-file snns-ls))))
    (if (probe-file "makefile")
      (port:run-prog "make"
        :args (list "--quiet")
        :output *standard-output*)
      (compile-c-file (c-source-file snns-ls) (library-file snns-ls)))
    (sb-posix:chdir cur-dir)))

)
