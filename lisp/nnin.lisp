(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :adopt :with-user-abort) :silent t))

(defpackage :nnin
  (:use :cl)
  (:local-nicknames (#:a #:alexandria))
  (:export :toplevel))

(in-package :nnin)

(defparameter *version* "0.0.1")

(defparameter k1-nums '(3 7 6 1 8 9 4 5 2))
(defparameter k2-nums '(5 4 3 2 7 6 5 4 3 2))

(defun k1 (&rest args)
  ;; Returns NIL when REST is 10, as it is undefined
  (let ((rest (mod (apply #'+ (mapcar #'* k1-nums args)) 11)))
    (unless (= rest 1)
      (if (zerop rest)
          0
          (- 11 rest)))))

(defun k2 (&rest args)
  ;; Returns NIL when REST is 10, as it is undefined
  (let ((rest (mod (apply #'+ (mapcar #'* k2-nums args)) 11)))
    (unless (= rest 1)
      (if (zerop rest)
          0
          (- 11 rest)))))

(defun two-random-numbers (range &optional (fresh t))
  (let ((random-state (make-random-state fresh)))
    (multiple-value-list (truncate (1+ (random range random-state)) 10))))

(defun day (&optional (fresh t))
  (two-random-numbers 31 fresh))

(defun month (&optional (fresh t))
  (two-random-numbers 12 fresh))

(defun year (&optional (fresh t))
  (two-random-numbers 99 fresh))

(defun birth-date (&optional (fresh t))
  (append (day fresh) (month fresh) (year fresh)))

(defun individual-numbers (&optional (fresh t))
  (let ((number (random 500 (make-random-state fresh))))
    (list (truncate number 100)
          (mod (truncate number 10) 10)
          (mod number 10))))

(defun nnin ()
  "Generate a Norwegian National Identity Number, using randomized values.
Should be used only to mock up test data.  If either `k1' or `k2' returns NIL,
do it again."
  (a:when-let*
      ((nnin (append (birth-date) (individual-numbers)))
       (k1 (apply #'k1 nnin))
       (x (append nnin (list k1)))
       (k2 (apply #'k2 x))
       (res (append x (list k2)))
       (result (concatenate 'string (mapcar #'digit-char res))))
    result))


;;; UI

(adopt:define-string *help-text*
  "nnin will generage and display a randomly generated norwegian national identity number.~@
   ~@
   This program was made to make generation of test clients easier when testing ~
   and debugging.")

(defparameter *examples*
  '(("Just run the command:"
     . "nnin")
    ("You can see help:"
     . "nnin -h")
    ("Or the version:"
     . "nnin -v")))


(defparameter *option-help*
  (adopt:make-option 'help
    :help "Display this help and exit."
    :long "help"
    :short #\h
    :reduce (constantly t)))

(defparameter *option-version*
  (adopt:make-option 'version
    :help "Display version information and exit."
    :long "version"
    :short #\v
    :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
    :name "nnin"
    :usage "[OPTIONS]"
    :summary "generate and display random norwegian national identity numbers"
    :help *help-text*
    :examples *examples*
    :contents (list *option-help*
                    *option-version*)))

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case
       (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (adopt:exit 130))))

(defun toplevel ()
  (exit-on-ctrl-c
    (handler-case
        (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
          (declare (ignorable arguments))
          (cond ((gethash 'help options) (adopt:print-help-and-exit *ui* :option-width 24))
                ((gethash 'version options) (write-line *version*) (adopt:exit)))
          (a:if-let ((nnin (nnin)))
            (format t "~a~%" nnin)
            (format t "One of the checksums failed - run again~%")))
      (error (c) (adopt:print-error-and-exit c)))))
