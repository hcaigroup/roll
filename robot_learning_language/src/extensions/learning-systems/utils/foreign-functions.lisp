(in-package :roll)

; compile-c-file
(defun compile-c-file (c-path out-path)
  (port:run-prog "/usr/bin/gcc"
    :args (list "-pipe" "-O3" "-fPIC" "-lm" "-shared" "-o"
                (namestring out-path)
                (namestring c-path))
    :output *standard-output*))
;; *standard-output* not tested, previously: (stdout) but this function is not defined any more
