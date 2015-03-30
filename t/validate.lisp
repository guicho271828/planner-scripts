
(in-package :pddl.test)
(in-suite :pddl.planner-scripts)

(test validate
  (let ((*default-pathname-defaults*
         (merge-pathnames "data/" #.*compile-file-truename*)))
    (is-true
      (validate-plan "domain.pddl"
                     "problem.pddl"
                     "opt"))
    (is-false
     (validate-plan "domain.pddl"
                    "problem.pddl"
                    "opt.dummy"))))
