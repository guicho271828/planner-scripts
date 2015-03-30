(in-package :pddl)
(cl-syntax:use-syntax :annot)

(defvar *default-fd-dir* "~/repos/downward/")

@export
(defun validate-plan (domain-pathname
                      problem-pathname
                      plan-pathname
                      &key
                        verbose
                        (stream *standard-output*))
  (handler-return ((uiop/run-program:subprocess-error (constantly nil)))
    (let ((string (with-output-to-string (string-stream)
                    (let ((broadcast (if verbose
                                         (make-broadcast-stream stream string-stream)
                                         string-stream)))
                      (run `(,(merge-pathnames #p"src/validate"
                                               (or (sb-ext:posix-getenv "FD_DIR")
                                                   *default-fd-dir*))
                              ,@(when verbose '(-v))
                              ,(merge-pathnames domain-pathname)
                              ,(merge-pathnames problem-pathname)
                              ,(merge-pathnames plan-pathname))
                           :show verbose
                           :output broadcast)))))
      ;; NOTE: this implementation is required, because VAL returns 0 when
      ;; the plan description is malformed.
      ;; We only trust its result when it outputs "Plan valid".
      (scan "Plan valid" string))))
