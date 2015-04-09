(in-package :pddl)
(cl-syntax:use-syntax :annot)

(defun search-fd ()
  (labels ((rec (path)
             (let ((downward (merge-pathnames "downward/" path)))
               (if (probe-file downward)
                   downward
                   (let ((parent (truename (merge-pathnames "../" path))))
                     (if (equal '(:absolute) (pathname-directory parent))
                         (warn "Fast Downward was not found!")
                         (rec parent)))))))
    (rec
     (asdf:system-source-directory 
      :pddl.planner-scripts))))

(defvar *default-fd-dir* (search-fd))

@export
(defun validate-plan (domain-pathname
                      problem-pathname
                      plan-pathname
                      &key
                        verbose
                        (stream *standard-output*))
  (let* ((command (format nil "~a/src/validate ~:[~;-v~] ~a ~a ~a"
                          (pathname-as-file
                           (or (sb-ext:posix-getenv "FD_DIR")
                               *default-fd-dir*))
                          verbose
                          (merge-pathnames domain-pathname)
                          (merge-pathnames problem-pathname)
                          (merge-pathnames plan-pathname)))
         (str (eazy-process:shell-command command :verbose verbose)))
    (when verbose (pprint str stream))
    (scan "Plan valid" str)))
