
(load 'unit-test.lisp)

(defpackage :test-unit-test
  (:use :common-lisp
        :unit-test))

(in-package :test-unit-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Public Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest null-test)
(deftest some-code
         (+ 1 1))
(deftest test-assert-pass
         (assert t))
(deftest test-assert-equal-pass
         (assert-equal 1 1))

(deftest test-test-fail
         (assert-failure 
           (test-fail "testing test-fail")))

(deftest test-assert-failure
         (assert-failure 
           (test-fail "testing assert-failure")))

(deftest test-assert-failure-fail
         (assert-failure
           (assert-failure 
              nil)))

(deftest test-assert-error
         (assert-error 
           (assert nil nil "testing test-error")))

(deftest test-assert-error-no-error
         (assert-failure
           (assert-error nil )))

(deftest test-assert-fail
         (assert-error
           (assert nil nil "Testing failure")))

(deftest test-assert-equal-fail
         (assert-failure
           (assert-equal 1 0 "Testing failure")))

(deftest test-assert-equal-integer-float
         (assert-equal 1 1.0 "float integer" :test #'=))

(deftest test-assert-equal-integer-float-fail
         (assert-failure
           (assert-equal 1 1.0 )))

(deftest test-assert-not-equal 
         (assert-not-equal 1 2))

(deftest test-assert-not-equal-fail 
         (assert-failure
           (assert-not-equal 1 1)))

(deftest test-test-assert
         (test-assert t "pass"))

(deftest test-test-assert-fail
         (assert-failure
           (test-assert nil "fail")))

(deftest test-test-assert-test
         (test-assert (= 1 1) "comparing 1 to 1"))

(deftest test-test-assert-test-fail
         (assert-failure
           (test-assert (= 1 2) "comparing 1 to 2")))

(deftest test-package
         (assert-equal (find-package :test-unit-test) *package* ))


(deftest test-is-in-package
         (test-assert (funcall (is-in-package :test-unit-test) (deftest hey) ) "testing-is-in-package" ))

(deftest test-is-in-package-fail
         (assert-failure 
            (test-assert (funcall (is-in-package :unit-test) (deftest hey) ) "testing-is-in-package-fail" )))

(setf test-variable 5)

(deftest test-external-variable
         (assert-equal test-variable 5))

(deftest test-external-variable-2
         (setf test-variable 6)
         (assert-equal test-variable 6))


(defun test-function ()
  (test-fail "old definition"))

(deftest test-redefine-function
         (flet ((test-function ()
           2))
         (assert-equal (test-function) 2)))

(deftest test-redefine-function-2
         (assert-failure (test-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Private Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :unit-test)

(deftest null-test)

(deftest test-all-tests
         (test-assert (all-tests (deftest hey)) "a" ))

(deftest test-deftest
         (let ((test (deftest hey)))
           (with-slots (test-name test-package test-code test-function) test
            (test-assert (typep test 'unit-test::test))
            (assert-equal test-name 'hey )
            (assert-equal test-package (find-package :unit-test))
            (assert-equal test-code nil)
            (test-assert (functionp test-function)))))

(deftest test-make-test-function-create
         (test-assert (functionp (make-test-function 'test nil))))

(deftest test-make-test-function-run
         (test-assert (not (funcall (make-test-function 'test nil)))))

(deftest test-make-test-function-run-fail
         (test-assert (typep (funcall (make-test-function 'test ((test-assert nil)))) 'unit-test::unit-test-failure )))

(deftest test-run-test 
         (let ((result (make-instance 'unit-test::test-result)))
            (run-test (deftest hey (test-assert nil)) result)
            (assert-equal (length (slot-value result 'sub-results)) 1)
            (let ((sub-result (first (slot-value result 'sub-results))))
              (test-assert (typep sub-result 'unit-test::test-result))
              (test-assert (not (null (slot-value sub-result 'test-error))))
              (test-assert (typep (slot-value sub-result 'test-error) 'unit-test::unit-test-failure)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :test-unit-test)


(setf results (run-tests))

(print results)
(format t "~&Tests: ~a" (number-of-tests results))


