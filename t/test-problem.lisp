(in-package :pddl.test)

(def-suite :pddl.planner-scripts :in :pddl)
(in-suite :pddl.planner-scripts)

(defun ensure-deleted (path)
  (when (probe-file path)
    (delete-file path)))

(test test-problem-fd
  (let ((*default-pathname-defaults*
         #.*compile-file-truename*))
    (multiple-value-bind (plan-path-list
                          search-time
                          search-memory
                          complete?)
        (test-problem-common
         (merge-pathnames "data/problem.pddl")
         (merge-pathnames "data/domain.pddl")
         :name "fd-clean"
         :hard-time-limit 1
         :verbose t)
      (is (not (null plan-path-list)))
      (is (numberp (print search-time)))
      (is (numberp (print search-memory)))
      (is-true complete?)
      (ensure-deleted (merge-pathnames "data/problem.negative"))
      (mapcar #'delete-file plan-path-list))))

(test test-problem-ff
  (let ((*default-pathname-defaults*
         #.*compile-file-truename*))
    (multiple-value-bind (plan-path-list
                          search-time
                          search-memory
                          complete?)
        (test-problem-common
         (merge-pathnames "data/problem.pddl")
         (merge-pathnames "data/domain.pddl")
         :name "ff-clean"
         :verbose t)
      (is (not (null plan-path-list)))
      (is (numberp (print search-time)))
      (is (numberp (print search-memory)))
      (is-false complete?)
      (ensure-deleted (merge-pathnames "data/problem.negative"))
      (mapcar #'delete-file plan-path-list))))

(test test-problem-ff-noverbose
  (let ((*default-pathname-defaults*
         #.*compile-file-truename*))
    (multiple-value-bind (plan-path-list
                          search-time
                          search-memory
                          complete?)
        (test-problem-common
         (merge-pathnames "data/problem.pddl")
         (merge-pathnames "data/domain.pddl")
         :name "ff-clean")
      (is (not (null plan-path-list)))
      (is (numberp (print search-time)))
      (is (numberp (print search-memory)))
      (is-false complete?)
      (ensure-deleted (merge-pathnames "data/problem.negative"))
      (mapcar #'delete-file plan-path-list))))

(test test-problem-marvin2
  (let ((*default-pathname-defaults*
         #.*compile-file-truename*))
    (multiple-value-bind (plan-path-list
                          search-time
                          search-memory
                          complete?)
        (test-problem-common
         (merge-pathnames "data/problem.pddl")
         (merge-pathnames "data/domain.pddl")
         :name "marvin2-clean"
         :verbose t)
      (is (not (null plan-path-list)))
      (is (numberp (print search-time)))
      (is (numberp (print search-memory)))
      (is-false complete?)
      (ensure-deleted (merge-pathnames "data/problem.negative"))
      (mapcar #'delete-file plan-path-list))))

(test test-problem-marvin2-noverbose
  (let ((*default-pathname-defaults*
         #.*compile-file-truename*))
    (multiple-value-bind (plan-path-list
                          search-time
                          search-memory
                          complete?)
        (test-problem-common
         (merge-pathnames "data/problem.pddl")
         (merge-pathnames "data/domain.pddl")
         :name "marvin2-clean")
      (is (not (null plan-path-list)))
      (is (numberp (print search-time)))
      (is (numberp (print search-memory)))
      (is-false complete?)
      (ensure-deleted (merge-pathnames "data/problem.negative"))
      (mapcar #'delete-file plan-path-list))))
