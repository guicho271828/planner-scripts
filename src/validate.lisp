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

(defun fd-relative-pathname (path)
  (let ((path (merge-pathnames
               path
               (truename
                (or (uiop:getenv "FD_DIR")
                    (search-fd))))))
    (assert (probe-file path))
    path))

@export
(defun validate-plan (domain-pathname
                      problem-pathname
                      plan-pathname
                      &key
                        verbose
                        (stream *standard-output*))
  (let* ((command (format nil "~a ~:[~;-v~] ~a ~a ~a"
                          (handler-case (fd-relative-pathname "validate")
                            (error ()
                               (fd-relative-pathname "src/validate")))
                          verbose
                          (merge-pathnames domain-pathname)
                          (merge-pathnames problem-pathname)
                          (merge-pathnames plan-pathname))))
    (when verbose (pprint command stream))
    (let ((str (uiop:run-program command :output :string)))
      (when verbose (pprint str stream))
      (ppcre:scan "Plan valid" str))))

(macrolet (($ (&rest args)
             `(uiop:run-program ,@args :output '(:string :stripped t))))

  @export
  (defun fd-translate (domain problem &key verbose)
    (let ((tmp (pathname-as-directory ($ "mktemp -d"))))
      ((lambda (str)
         (when verbose (princ str)))
       ($ (format nil "cd ~a; ~a ~a ~a"
                  tmp
                  (handler-case (fd-relative-pathname "translate/translate.py")
                    (error ()
                      (fd-relative-pathname "src/translate/translate.py")))
                  (merge-pathnames domain)
                  (merge-pathnames problem))))
      (merge-pathnames "output.sas" tmp)))

  @export
  (defun fd-preprocess (path &key verbose)
    (let ((tmp (pathname-as-directory ($ "mktemp -d"))))
      (with-open-file (s path)
        ((lambda (str)
           (when verbose (princ str)))
         ($ (format nil "cd ~a; ~a"
                    tmp
                    (handler-case (fd-relative-pathname "preprocess/preprocess")
                      (error ()
                        (fd-relative-pathname "src/preprocess/preprocess"))))
            :input s))
        (merge-pathnames "output" tmp)))))
