(in-package :pddl)
(cl-syntax:use-syntax :annot)

;;;; wrapper functions

(defun ulimit (rlimit)
  (case rlimit
    ((:infinity) "unlimited")
    (t rlimit)))

(defun wrap-option (string)
  (format nil "~{\"~a\" ~}" (ppcre:split " " string)))

;;;; parameters

(defvar *system* (asdf:system-source-directory :pddl.planner-scripts))

(defparameter *limitsh*
  (merge-pathnames "limit.sh" *system*))

@export
(defparameter *memory-limit*
  (cl-rlimit:rlimit cl-rlimit:+rlimit-address-space+))

@export
(defparameter *soft-time-limit*
  (cl-rlimit:rlimit cl-rlimit:+rlimit-cpu-time+))

@export
(defparameter *hard-time-limit*
  (cl-rlimit:rlimit cl-rlimit:+rlimit-cpu-time+))

(defvar *verbose* nil)

;;;; helpers
@export
(define-condition plan-not-found (warning)
  ((problem-path :initarg :problem-path)
   (domain-path :initarg :domain-path))
  (:report (lambda (c s)
             (with-slots (problem-path domain-path) c
                (format s "Failed to find a plan! ~a"
                        (uiop:pathname-directory-pathname
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

(defun %signal (signo)
  (format t "~&received ~A~%" (trivial-signal:signal-name signo))
  (signal 'trivial-signal:unix-signal :signo signo)
  (format t "~&Condition not handled, Exiting.")
  (error "~&Condition not handled, Exiting."))

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
                              (hard-time-limit *hard-time-limit*)
                            &aux (*verbose* verbose))
  (declare (ignore time-limit))
  (unless (probe-file *limitsh*)
    (error "Failed to find limit.sh! Did you moved the directory? ~a "
           *limitsh*))
  (let ((problem (pathname problem))
        (domain (pathname domain))
        (*print-case* :downcase))
    (fresh-line)
    (restart-case
        (trivial-signal:signal-handler-bind ((:int #'%signal)
                                             (:xcpu #'%signal)
                                             (:term #'%signal)
                                             (:usr1 #'%signal))
          (uiop:run-program (mapcar #'princ-to-string
                                    `(,*limitsh*
                                      -m ,(ulimit memory)
                                      -t ,(ulimit hard-time-limit)
                                      ,@(when iterated `(-i))
                                      ,@(when verbose `(-v))
                                      ,@(when options `(-o ,options))
                                      -- ,name ,problem ,domain))
                            :ignore-error-status t
                            :output t
                            :error-output t)
          (invoke-restart
           (find-restart 'finish)))
      (finish ()
        (when *verbose* (format t "~&Running finalization"))
        (find-plans-common domain problem)))))

;;;; reading the results
;; limit.sh writes to a specific output file, so read the result.
;; the `stream' argument in test-problem merely provides a verbose
;; printing, and the output result is not used in these result analysers.

(defun common-memory (problem)
  (or
   (handler-case
       (with-open-file (s (make-pathname :type "stat" :defaults problem))
         (iter (for line = (read-line s nil nil))
               (while line)
               (match line
                 ((trivia.ppcre:ppcre ".*maxmem\\s+([.0-9]+)" num)
                  (leave (parse-integer num :junk-allowed t))))))
     (file-error () nil))
   -1))

(defun common-time (problem)
  (or
   (handler-case
       (with-open-file (s (make-pathname :type "stat" :defaults problem))
         (iter (for line = (read-line s nil nil))
               (while line)
               (match line
                 ((trivia.ppcre:ppcre ".*cputime\\s+([.0-9]+)" num)
                  (leave (parse-integer num :junk-allowed t))))))
     (file-error () nil))
   -1))

(defun common-complete (problem)
  (probe-file
   (make-pathname :type "negative"
                  :defaults problem)))

(defun common-plans (problem)
  (sort (remove-duplicates
         (append
          (directory
           (make-pathname :name (format nil "~a.plan" (pathname-name problem))
                          :type :wild
                          :defaults problem))
          (directory
           (make-pathname :type "plan"
                          :defaults problem)))
         :test #'string= :key #'namestring)
        #'string> :key #'pathname-type))

(defun find-plans-common (domain problem)
  @ignorable domain
  (let ((plans (common-plans problem))
        (complete (common-complete problem)))
    (when complete
      (format t "~&Problem UNSAT"))
    (when (and (not plans) (not complete))
      (signal 'plan-not-found))
    (values
     plans
     (let ((time (common-time problem)))
       (format t "~&Time spent: ~,1f" (float (/ time 1000)))
       time)
     (common-memory problem)
     complete)))
