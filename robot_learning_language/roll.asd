#+sbcl (sb-ext:with-unlocked-packages (sb-mop) (rename-package 'sb-mop 'sb-mop '(clos mop)))

(defsystem roll
  :depends-on (roslisp #:cram-language
                       #:designators
                       #:alexandria
                       #:cffi
                       #:clsql
                       #:md5
                       #:uffi
                       #:clsql-uffi
                       #:clsql-mysql
                       #:port
                       #:cl-libsvm)
  :components
  ( (:file "package")
    (:module "src"
      :components
      ( (:file "cram-tasks")
        (:module "problem-generation"
          :components ( (:file "samples") ))
        (:module "experience"
          :serial T
          :components ( (:module "experience-utils"
                          :serial T
                          :components
                            ( (:file "misc")
                              (:file "recorder")
                              (:file "occurrence-handling") ))
                        (:file "experience-automata")
                        (:file "experience")
                        (:module "raw-experience"
                          :components
                          ( (:file "parse")
                            (:file "definition") )
                          :serial T)
                        (:module "abstract-experience"
                          :components
                          ( (:file "parse")
                            (:file "definition")
                            (:file "convert") )
                          :serial T
                          :depends-on ("experience" "experience-utils")) )
          :serial T)
        (:module "learning-problem"
          :components
          ( (:file "learning-problem-class")
            (:file "input-conversion")
            (:file "learning-problem") )
          :serial T)
        (:file "learning-system" :depends-on ("experience" "learning-problem"))
        (:module "extensions"
          :components ( (:module "experience"
                          :components
                          ( (:file "format-experience")
                            (:module "database-experience"
                              :components ( (:file "database-utils")
                                            (:file "class")
                                            (:file "retrieve-experience")
                                            (:file "deliver-experience")
                                            (:file "update-experience") )
                              :serial T) ))
                        (:file "learning-problem-classes")
                        (:module "learning-systems"
                          :depends-on ("learning-problem-classes")
                          :components
                          ( (:module "utils"
                              :components
                              ( (:file "foreign-functions")
                                (:file "generic")
                                (:file "lp-integration-templates") ))
                            (:module "generic-ls"
                              :components
                              ( (:file "learning-system")
                                (:file "experience")
                                (:file "generic-learning")
                                (:file "integration") )
                              :depends-on ("utils")
                              :serial T)
                            (:module "weka"
                              :components
                              ( (:file "learning-system")
                                (:file "experience")
                                (:file "weka-learning")
                                (:file "integration")
                                (:file "integration-weka-tree")
                                (:file "integration-weka-svm") )
                              :depends-on ("utils")
                              :serial T)
                            (:module "libsvm"
                              :components
                              ( (:file "learning-system")
                                (:file "experience")
                                (:file "libsvm-learning")
                                (:file "integration") )
                              :depends-on ("utils")
                              :serial T)
                            (:module "snns"
                              :components
                              ( (:file "learning-system")
                                (:file "experience")
                                (:file "snns-learning")
                                (:file "integration") )
                              :depends-on ("utils")
                              :serial T)  )) )) )
      :serial T
      :depends-on ("package")) ))


