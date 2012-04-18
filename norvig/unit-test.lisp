
(defpackage :unit-test
  (:use :common-lisp)
  (:export :deftest
           :run-tests
           :is-in-package
           :test-assert
           :test-fail
           :number-of-tests
           :assert-failure
           :assert-error
           :assert-not-equal
           :assert-predicate
           :assert-equal))

(in-package :unit-test)

(define-condition unit-test-failure (simple-error)
  ()) 

(defmacro test-assert (test &optional (message "" ))
  `(assert ,test nil 'unit-test-failure :format-control "~a~&test-assert failure in:~a" :format-arguments (list ,message ',test)))

(defun assert-predicate (test predicate &optional (message ""))
  (test-assert (funcall predicate test) message))

(defun assert-equal (actual expected &optional (message "") &key (test #'equal)) 
    (let ((message (format nil "~a~&assert-equal failure: ~&Expected: ~a~&Actual: ~a~&Comparison: ~a" message actual expected test)))
  (test-assert (funcall test actual expected) message)))

(defun assert-not-equal (actual expected &optional (message "") &key (test #'equal)) 
    (let ((message (format nil "~a~&assert-not-equal failure: ~&Not Expected: ~a~&Actual: ~a~&Comparison: ~a" message actual expected test)))
  (test-assert (not (funcall test actual expected)) message)))

(defun test-fail (&optional (message ""))
    (let ((message (format nil "~a~&test-fail" message)))
  (test-assert nil message)))

(defmacro assert-failure (&body body)
  `(handler-case
     (progn
       ,@body)
     (unit-test-failure (e) nil)
     (error (e) (test-fail (format nil "~&assert-failure failure: error in: ~a~&Error: ~a" ',body e)))
     (:no-error (x) (test-fail (format nil "~&assert-failure failure: no failure in: ~a" ',body)))))

(defmacro assert-error (&body body)
  `(handler-case
     (progn
       ,@body)
     (error (e) nil)
     (:no-error (x) (test-fail (format nil "~&assert-error failure: no error in: ~a" ',body)))))

(defvar *tests* nil)

(defclass test ()
  ((test-name
     :initarg :test-name)
   (test-package
     :initarg :test-package)
   (test-code
     :initarg :test-code)
   (test-function
     :initarg :test-function)))

(defclass test-result ()
  ((test
     :initarg :test
     :initform nil)
   (test-error
      :initform nil)
   (sub-results 
     :initform nil)))

(defmethod test-full-name ((a-test test))
  (with-slots (test-name test-package) a-test
  (format nil "~a:~a" test-package test-name)))

(defmethod print-object ((a-test test) (stream t))
  (format stream "~&Test: ~a~&Package: ~a~&Code: ~a" (slot-value a-test 'test-name) (slot-value a-test 'test-package)
          (slot-value a-test 'test-code)))

(defmethod print-object ((a-result test-result) (stream t))
  (let ((failures 0))
  (with-slots (test test-error sub-results) a-result
      (unless (null test)
      (if (null test-error)
          (format stream "~&passed: ~a" (test-full-name test))
          (progn
          (format stream "~&FAILED: ~a~&Message: ~a~&~a" (test-full-name test) test-error test)
          (setf failures (+ failures 1)))))
      (if (consp sub-results)
          (dolist (result (reverse sub-results))
            (setf failures (+ failures(print-object result stream)))))
  (when (null test)
  (format stream "~&Failures: ~a" failures)))
  failures))

(defmethod number-of-tests ((a-result test-result))
  (let ((tests 0))
    (with-slots (sub-results) a-result
      (when (consp sub-results)
        (setf tests (+ tests (length sub-results)))
        (dolist (result sub-results)
          (setf tests (+ tests (number-of-tests result))))))
    tests))


(defmethod run-test ((a-test test) (a-result test-result)) 
  (let ((new-result (make-instance 'test-result :test a-test))
  (exception (funcall (slot-value a-test 'test-function))))
  (unless (null exception)
    (setf (slot-value new-result 'test-error) exception))
  (push new-result (slot-value a-result 'sub-results))))

(defmacro make-test-function (name body)
  `(lambda () 
     (handler-case
     (progn
     ,@body)
     (simple-condition (e) 
           (format t "~&FAILURE: ~a~&" ',name)
           (apply #'format t (simple-condition-format-control e)
                          (simple-condition-format-arguments e))
           e)
     (:no-error (x) nil))))

(defmacro deftest (name &body body)
  (assert (symbolp name) nil (format nil "name ~a must be a sumbol." name))
  `(let ((new-test (make-instance 'test :test-name ',name 
                        :test-package ',*package*
                        :test-code ',body
                        :test-function (make-test-function ,name ,body))))
     (push new-test *tests*)
     new-test))

(defun all-tests (test) t)

(defun is-in-package (package)
  (lambda (test)
    (equal (find-package package) (slot-value test 'test-package))))

(defun run-tests (&optional (predicate #'all-tests))
  (let ((results (make-instance 'test-result)))
  (dolist (test (reverse *tests*))
    (if (funcall predicate test)
    (run-test test results)))
   results))

