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

(defvar *verbose* nil)

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

(defun %signal (signo)
  (format t "~&received ~A~%" (trivial-signal:signal-name signo))
  (signal 'trivial-signal:unix-signal :signo signo)
  (format t "~&Condition not handled, Exiting.")
  (error "~&Condition not handled, Exiting.")
  ;; (sb-ext:exit :code 1 :abort t)
  )

;; this one may accidentally overwrite eazy-process:finalize-process.
;; this is not true, but in case when the symbol is imported in the future.
;; (defun finalize-process (process)
;;   (format t "~&Sending signal 15 to the test-problem process...")
;;   (force-output)
;;   (sb-ext:process-kill process 15) ; SIGTERM
;;   ;; (when (sb-ext:process-alive-p process)
;;   ;;   (sb-ext:process-wait process))
;;   (iter (while (sb-ext:process-alive-p process))
;;         (format t "~&waiting")
;;         (sleep 1)))


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
  (let ((problem (pathname problem))
        (domain (pathname domain))
        (*print-case* :downcase))
    (fresh-line)
    (handler-bind ((warning #'muffle-warning))
      (restart-case
          (trivial-signal:signal-handler-bind ((:int #'%signal)
                                               (:xcpu #'%signal)
                                               (:term #'%signal)
                                               (:usr1 #'%signal))
            (eazy-process:with-process
                (p (mapcar #'princ-to-string
                           `(,*limitsh*
                             -m ,(ulimit memory)
                             -t ,(ulimit hard-time-limit)
                             ,@(when iterated `(-i))
                             ,@(when verbose `(-v))
                             ,@(when options `(-o ,options))
                             -- ,name ,problem ,domain))
                   `(:in
                     (,(pathname (format nil "/proc/~a/fd/1" (eazy-process:getpid)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
                     (,(pathname (format nil "/proc/~a/fd/2" (eazy-process:getpid)))
                       :direction :output :if-exists :append :if-does-not-exist :create)))
              (eazy-process:wait p))
            (invoke-restart
             (find-restart 'finish)))
        (finish ()
          (when *verbose* (format t "~&Running finalization"))
          (find-plans-common domain problem))))))

;;;; reading the results
;; limit.sh writes to a specific output file, so read the result.
;; the `stream' argument in test-problem merely provides a verbose
;; printing, and the output result is not used in these result analysers.

(defun common-memory (problem)
  (parse-integer
   (eazy-process:shell-command
    (format nil
            "awk '/maxmem/{print $2; ok=1} END{if(!ok){print -1}}' ~a~a.stat || echo -1"
            (pathname-directory-pathname problem)
            (pathname-name problem))
    :verbose *verbose*)
   :junk-allowed t))

(defun common-time (problem)
  (parse-integer
   (eazy-process:shell-command
    (format nil
            "awk '/cputime/{print $2; ok=1} END{if(!ok){print -1}}' ~a~a.stat || echo -1"
            (pathname-directory-pathname problem)
            (pathname-name problem))
    :verbose *verbose*)
   :junk-allowed t))

(defun common-complete (problem)
  (probe-file
   (format nil "~a~a.negative"
           (pathname-directory-pathname problem)
           (pathname-name problem))))

(defun common-plans (problem)
  (sort (split "\\s+"
               (eazy-process:shell-command
                (format nil
                        "ls ~a~a.plan*"
                        (pathname-as-directory
                         (pathname-directory-pathname problem))
                        (pathname-name problem))
                :verbose *verbose*))
        #'string>))

(defun find-plans-common (domain problem)
  @ignorable domain
  (values
   (common-plans problem)
   (common-time problem)
   (common-memory problem)
   (common-complete problem)))
