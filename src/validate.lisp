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
  (let* ((command (format nil "~a ~:[~;-v~] ~a ~a ~a"
                          (merge-pathnames
                           "src/validate"
                           (truename
                            (or (sb-ext:posix-getenv "FD_DIR")
                                *default-fd-dir*)))
                          verbose
                          (merge-pathnames domain-pathname)
                          (merge-pathnames problem-pathname)
                          (merge-pathnames plan-pathname)))
         (str (eazy-process:shell-command command :verbose verbose)))
    (when verbose (pprint str stream))
    (scan "Plan valid" str)))

(defun normalize-as-dir (str)
  (let ((str (remove #\Newline str)))
    (if (char= #\/ (aref str (1- (length str))))
        str
        (concatenate 'string str "/"))))

@export
(defun fd-translate (domain problem &key verbose)
  (macrolet (($ (&rest args) `(eazy-process:shell-command ,@args :verbose verbose)))
    (let ((tmp (normalize-as-dir ($ "mktemp -d"))))
      ((lambda (str)
         (when verbose (princ str)))
       ($ (format nil "cd ~a; ~a ~a ~a"
                  tmp
                  (merge-pathnames
                   "src/translate/translate.py"
                   (truename
                    (or (sb-ext:posix-getenv "FD_DIR")
                        *default-fd-dir*)))
                  (merge-pathnames domain)
                  (merge-pathnames problem))))
      (merge-pathnames "output.sas" tmp))))

@export
(defun fd-preprocess (path &key verbose)
  (macrolet (($ (&rest args) `(eazy-process:shell-command ,@args :verbose verbose)))
    (let ((tmp (normalize-as-dir ($ "mktemp -d"))))
      (with-open-file (s path)
        ((lambda (str)
           (when verbose (princ str)))
         ($ (format nil "cd ~a; ~a"
                    tmp
                    (merge-pathnames
                     "src/preprocess/preprocess"
                     (truename
                      (or (sb-ext:posix-getenv "FD_DIR")
                          *default-fd-dir*))))
            :input s))
        (merge-pathnames "output" tmp)))))
