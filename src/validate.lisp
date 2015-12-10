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

(defun fd-relative-pathname (path)
  (merge-pathnames
   path
   (truename
    (or (sb-ext:posix-getenv "FD_DIR")
        *default-fd-dir*))))

@export
(defun validate-plan (domain-pathname
                      problem-pathname
                      plan-pathname
                      &key
                        verbose
                        (stream *standard-output*))
  (let* ((command (format nil "~a ~:[~;-v~] ~a ~a ~a"
                          (fd-relative-pathname "src/validate")
                          verbose
                          (merge-pathnames domain-pathname)
                          (merge-pathnames problem-pathname)
                          (merge-pathnames plan-pathname))))
    (when verbose (pprint command stream))
    (let ((str (uiop:run-program command)))
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
                  (fd-relative-pathname "src/translate/translate.py")
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
                    (fd-relative-pathname "src/preprocess/preprocess"))
            :input s))
        (merge-pathnames "output" tmp)))))
