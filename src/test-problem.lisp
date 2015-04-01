(in-package :pddl)
(cl-syntax:use-syntax :annot)

;;;; wrapper functions

(defun ulimit (rlimit)
  (case rlimit
    ((:infinity) "unlimited")
    (t rlimit)))

(defun wrap-option (string)
  (format nil "~{\"~a\" ~}" (split " " string)))

;;;; parameters

(defvar *system*
  (pathname-as-directory 
   (asdf:system-source-directory :pddl.planner-scripts)))

(defparameter *limitsh*
  (merge-pathnames "limit.sh" *system*))

@export
(defparameter *memory-limit*
  (rlimit +rlimit-address-space+))

@export
(defparameter *soft-time-limit*
  (rlimit +rlimit-cpu-time+))

@export
(defparameter *hard-time-limit*
  (rlimit +rlimit-cpu-time+))

;;;; helpers
@export
(define-condition plan-not-found (warning)
  ((problem-path :initarg :problem-path)
   (domain-path :initarg :domain-path))
  (:report (lambda (c s)
             (with-slots (problem-path domain-path) c
                (format s "Failed to find a plan! ~a"
                        (pathname-directory-pathname
                         problem-path))))))

;; http://www.ymeme.com/slurping-a-file-common-lisp-83.html
(defun slurp (stream)
  (let ((seq (make-array (file-length stream)
                         :element-type 'character
                         :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun read-file (path)
  (with-input-from-file (s path)
    (slurp s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pathname-p (path)
    "defined for optima matcher"
    (pathnamep path)))

@export
(define-condition unix-signal ()
  ((signo :initarg :signo :reader signo)))

(defun %signal (signo)
  (format t "~&received ~A~%" (signal-name signo))
  (signal 'unix-signal :signo signo)
  (format t "~&Condition not handled, Exiting.")
  (error "~&Condition not handled, Exiting.")
  ;; (sb-ext:exit :code 1 :abort t)
  )

(defun finalize-process (process verbose)
  (format t "~&Sending signal 15 to the test-problem process...")
  (force-output)
  (sb-ext:process-kill process 15) ; SIGTERM
  ;; (when (sb-ext:process-alive-p process)
  ;;   (sb-ext:process-wait process))
  (iter (while (sb-ext:process-alive-p process))
        (format t "~&waiting")
        (sleep 1)))

;;;; general planners

(export 'finish)

@export
(defun test-problem-common (problem domain
                            &key
                              (stream *standard-output*)
                              (error *error-output*)
                              options verbose iterated
                              (name (error "no planner name given!"))
                              (memory *memory-limit*)
                              (time-limit *soft-time-limit*)
                              (hard-time-limit *hard-time-limit*))
  (declare (ignore time-limit))
  (let ((problem (pathname problem))
        (domain (pathname domain))
        (*print-case* :downcase))
    (fresh-line)
    (restart-case
        (signal-handler-bind ((:int #'%signal)
                              (:xcpu #'%signal)
                              (:term #'%signal)
                              (:usr1 #'%signal))
          (break)
          (eazy-process:with-process
              (p (print
                  (mapcar #'princ-to-string
                          `(,*limitsh*
                            -m ,(ulimit memory)
                            -t ,(ulimit hard-time-limit)
                            ,@(when iterated `(-i))
                            ,@(when verbose `(-v))
                            ,@(when options `(-o ,options))
                            -- ,name ,problem ,domain)))
                 `(:in
                   (,(pathname (format nil "/proc/~a/fd/1" (eazy-process:getpid)))
                     :direction :output :if-exists :append :if-does-not-exist :create)
                   (,(pathname (format nil "/proc/~a/fd/2" (eazy-process:getpid)))
                     :direction :output :if-exists :append :if-does-not-exist :create)))
            (eazy-process:wait p))
          (invoke-restart
           (find-restart 'finish)))
      (finish ()
        (format t "~&Running finalization")
        (find-plans-common domain problem verbose)))))

;;;; reading the results
;; limit.sh writes to a specific output file, so read the result.
;; the `stream' argument in test-problem merely provides a verbose
;; printing, and the output result is not used in these result analysers.
(defun common-memory (problem)
  (block nil
    (ignore-errors
      (parse-integer
       (run `(pipe (grep "maxmem"
                         ,(format nil "~a~a.stat"
                                  (pathname-directory-pathname problem)
                                  (pathname-name problem)))
                   (cut -d " " -f 2))
            :output :string
            :on-error (lambda (c)
                        (declare (ignore c))
                        (return -1)))))))
(defun common-time (problem)
  (block nil
    (ignore-errors
      (parse-integer
       (run `(pipe (grep "cputime"
                         ,(format nil "~a~a.stat"
                                  (pathname-directory-pathname problem)
                                  (pathname-name problem)))
                   (cut -d " " -f 2))
            :output :string
            :on-error (lambda (c)
                        (declare (ignore c))
                        (return -1)))))))
(defun common-complete (problem)
  (probe-file
   (format nil "~a~a.negative"
           (pathname-directory-pathname problem)
           (pathname-name problem))))

(defun common-plans (problem verbose)
  (sort (block nil
          (run `(pipe (find ,(pathname-directory-pathname problem)
                            -maxdepth 1 -mindepth 1)
                      (progn (grep (,(pathname-name problem) .plan))
                             (true)))
               :show verbose :output :lines))
        #'string>))
(defun find-plans-common (domain problem verbose)
  @ignorable domain
  (values
   (common-plans problem verbose)
   (common-time problem)
   (common-memory problem)
   (common-complete problem)))
