
(defpackage :unit-test
  (:use :common-lisp)
  (:export :deftest
	   :clear-tests
           :run-tests
           :run-tests-with-name
	   :run-tests-in-package
	   :run-tests-current-package
           :is-in-package
           :assert-true
           :assert-nil
           :test-fail
           :number-of-tests
           :assert-failure
           :assert-error
           :assert-not-equal
           :assert-predicate
           :assert-equal
           :assert-equal-trim
           :assert-equal-by-line
           :assert-equal-mvl
           :test-name
           :unit-test-failure))

(in-package :unit-test)

(define-condition unit-test-failure (simple-error)
  ()) 

(defmacro assert-true (test &optional (message "" ))
  `(assert ,test nil 'unit-test-failure :format-control "~S~&assert-true failure in:~S" :format-arguments (list ,message ',test)))

(defmacro assert-nil (test &optional (message ""))
  `(assert-true (not ,test) ,message))

(defun assert-predicate (test predicate &optional (message ""))
  (assert-true (funcall predicate test) message))

(defun assert-equal (actual expected &optional (message "") &key (test #'equal)) 
    (let* ((message (format nil "~a~&assert-equal failure: ~&Actual ~S~&Expected ~S~&Comparison: ~S" message actual expected test)))
  (assert-true (funcall test actual expected) message)))

(defmacro assert-equal-mvl (form list)
  `(assert-equal (multiple-value-list ,form) ,list))

(defun assert-not-equal (actual expected &optional (message "") &key (test #'equal)) 
    (let ((message (format nil "~S~&assert-not-equal failure: ~&Not Expected: ~S~&Actual: ~S~&Comparison: ~S" message actual expected test)))
  (assert-true (not (funcall test actual expected)) message)))

(defun assert-equal-trim (actual expected &optional (message ""))
    (let* ((test #'equal)
           (message (format nil "~a~&assert-equal-trim failure: ~&Actual ~a~&Expected ~a~&Comparison: ~S" message actual expected test)))
  (assert-true (funcall test (string-trim '(#\Space #\Tab #\Newline) actual ) (string-trim '(#\Space #\Tab #\Newline) expected)) message)))

(defun assert-equal-by-line (actual expected &optional (message ""))
  (let ((actualLines (split-sequence:split-sequence #\Newline actual))
        (expectedLines (split-sequence:split-sequence #\Newline expected))
        (expectedLine ""))
    (dolist (actualLine actualLines)
      (setf expectedLine "")
      (setf actualLine (string-trim '(#\Space #\Tab) actualLine))
      (when (not (equal "" actualLine))
        (util:while (equal "" expectedLine)
                    (if (null expectedLines)
                      (test-fail (format nil "~a~%Too many actual lines ~{ ~%~a ~}" message actualLines)))
                    (setf expectedLine (string-trim '(#\Space #\Tab) (first expectedLines)))
                    (setf expectedLines (rest expectedLines)))
        (if (and (not (equal "" actualLine)) (not (equal "" expectedLine)))
          (assert-equal-trim actualLine expectedLine (format nil "~a~%assert-equal-by-line failure: ~%Actual:~a ~%Expected: ~a" message actual expected)))))
    (if (remove-if #'(lambda (x) 
                       (equal "" (string-trim '(#\Space #\Tab) x))) expectedLines)
      (test-fail (format nil "~a~%Missing expected lines:~{ ~%~a ~}" message expectedLines)))))



(defun test-fail (&optional (message ""))
    (let ((message (format nil "~a~&test-fail" message)))
  (assert-true nil message)))

(defmacro assert-failure (&body body)
  `(handler-case
     (progn
       ,@body)
     (unit-test-failure (e)
                        (declare (ignore e))
                        nil)
     (error (e) (test-fail (format nil "~&assert-failure failure: error in: ~S~&Error: ~S" ',body e)))
     (:no-error (x)
                (declare (ignore x))
                (test-fail (format nil "~&assert-failure failure: no failure in: ~S" ',body)))))

(defmacro assert-error (&body body)
  `(handler-case
     (progn
       ,@body)
     (error (e) 
            (declare (ignore e))
            nil)
     (:no-error (x) 
                (declare (ignore x))
                (test-fail (format nil "~&assert-error failure: no error in: ~S" ',body)))))

(defvar *tests* nil)

(defun clear-tests ()
  (setf *tests* nil))

(defclass test ()
  ((test-name
     :initarg :test-name
     :accessor test-name)
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

(defgeneric test-full-name (a-test))

(defmethod test-full-name ((a-test test))
  (with-slots (test-name test-package) a-test
  (format nil "~S:~S" test-package test-name)))

(defmethod print-object ((a-test test) (stream t))
  (format stream "~&Test: ~S~&Package: ~S~&Code: ~S" (slot-value a-test 'test-name) (slot-value a-test 'test-package)
          (slot-value a-test 'test-code)))

(defmethod print-object ((a-result test-result) (stream t))
  (let ((failures 0))
  (with-slots (test test-error sub-results) a-result
      (unless (null test)
      (if (null test-error)
          (format stream "~&passed: ~S" (test-full-name test))
          (progn
          (format stream "~&FAILED: ~a~&Message: ~a~&~a" (test-full-name test) test-error test)
          (setf failures (+ failures 1)))))
      (if (consp sub-results)
          (dolist (result (reverse sub-results))
            (setf failures (+ failures(print-object result stream)))))
  (when (null test)
  (format stream "~&Failures: ~S" failures)))
  failures))

(defgeneric number-of-tests (a-result))

(defmethod number-of-tests ((a-result test-result))
  (let ((tests 0))
    (with-slots (sub-results) a-result
      (when (consp sub-results)
        (setf tests (+ tests (length sub-results)))
        (dolist (result sub-results)
          (setf tests (+ tests (number-of-tests result))))))
    tests))

(defgeneric run-test (a-test a-result))

(defmethod run-test ((a-test test) (a-result test-result)) 
  (let ((new-result (make-instance 'test-result :test a-test)))
     (handler-case
     (funcall (slot-value a-test 'test-function))
     (simple-condition (exception) 
           (setf (slot-value new-result 'test-error) exception))
     (:no-error (&rest x)
                (declare (ignore x))
                nil))
  (push new-result (slot-value a-result 'sub-results))))

(defmacro make-test-function (name body)
  `(setf (symbol-function ',name) #'(lambda () ,@body)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun add-test (name package test)
  (let* ((symbol (symb (package-name package) '--- name))
         (loc (assoc symbol *tests*)))
    (if loc
      (setf (second loc) test)
      (setf *tests* (cons (list symbol test) *tests*)))))

(defmacro deftest (name &body body)
  (assert (symbolp name) nil (format nil "name ~S must be a sumbol." name))
  (let ((gnew-test (gensym)))
  `(let ((,gnew-test (make-instance 'test :test-name ',name 
                        :test-package ',*package*
                        :test-code ',body
                        :test-function (make-test-function ,name ,body))))
     (add-test ',name ',*package* ,gnew-test)
     ,gnew-test)))

(defun all-tests (test) 
  (declare (ignore test))
  t)

(defun is-in-package (package)
  (lambda (test)
    (equal (find-package package) (slot-value test 'test-package))))

(defun run-tests (&optional (predicate #'all-tests))
  (let ((results (make-instance 'test-result)))
  (dolist (name-test (reverse *tests*))
    (if (funcall predicate (second name-test))
    (run-test (second name-test) results)))
   results))


(defun run-tests-with-name (name)
  (run-tests #'(lambda (x)
                 (eql (test-name x) name))))
                 

(defun run-tests-in-package (package)
  (run-tests (is-in-package package)))

(defun run-tests-current-package ()
  (run-tests (is-in-package *package*)))
