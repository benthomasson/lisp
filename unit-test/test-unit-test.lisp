
(defpackage :test-unit-test
  (:use :common-lisp
        :unit-test))

(in-package :unit-test)

(defun return-values-fn (x y z)
  (values x y z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (macroexpand-1 '(deftest null-test)))

(print (macroexpand-1 '(deftest some-code (+ 1 1))))

(print (macroexpand-1 '(make-test-function some-code ((+ 1 1)))))

(print (macroexpand-1 '(assert-true (eql 1 1) "something")))

(print (macroexpand-1 '(assert-failure (eql 1 1) "something")))

(in-package :test-unit-test)

(defun return-values-fn (x y z)
  (values x y z))

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
         (assert-true t "pass"))

(deftest test-test-assert-fail
         (assert-failure
           (assert-true nil "fail")))

(deftest test-test-assert-test
         (assert-true (= 1 1) "comparing 1 to 1"))

(deftest test-test-assert-test-fail
         (assert-failure
           (assert-true (= 1 2) "comparing 1 to 2")))

(deftest test-package
         (assert-equal (find-package :test-unit-test) *package* ))


(deftest test-is-in-package
         (assert-true (funcall (is-in-package :test-unit-test) (deftest hey) ) "testing-is-in-package" ))

(deftest test-is-in-package-fail
         (assert-failure 
            (assert-true (funcall (is-in-package :unit-test) (deftest hey) ) "testing-is-in-package-fail" )))

(let ((test-variable 5))
(deftest test-external-variable
         (assert-equal test-variable 5))

(deftest test-external-variable-2
         (setf test-variable 6)
         (assert-equal test-variable 6)))

(defun test-function ()
  (test-fail "old definition"))

(deftest test-redefine-function
         (flet ((test-function ()
           2))
         (assert-equal (test-function) 2)))

(deftest test-redefine-function-2
         (assert-failure (test-function)))

(deftest test-simple-test
         (assert-true t))

(deftest test-call-simple-test
         (test-simple-test)
         (test-simple-test))

(deftest test-fail-test
         (assert-true nil))

(deftest test-call-fail-test
         (test-call-simple-test)
         (assert-failure (test-fail-test))
         (assert-failure (test-fail-test)))

(deftest test-pop-assert-equal
         (let ((a-list '(a b c)))
         (assert-equal a-list '(a b c))
         (pop a-list)
         (assert-equal a-list '(b c))
         (setf a-list '(a b c))
         (assert-equal (pop a-list) 'a)))

(print (macroexpand-1 '(deftest test-values
         (return-values-fn 1 2 3))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Private Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :unit-test)

(deftest null-test)

(deftest test-all-tests
         (assert-true (all-tests (deftest hey)) "a" ))

(deftest test-deftest
         (let ((test (deftest hey)))
           (with-slots (test-name test-package test-code test-function) test
            (assert-true (typep test 'unit-test::test))
            (assert-equal test-name 'hey )
            (assert-equal test-package (find-package :unit-test))
            (assert-equal test-code nil)
            (assert-true (functionp test-function)))))

(deftest test-make-test-function-create
         (assert-true (functionp (make-test-function test nil))))

(format t "~%make-test-function: ~S" (macroexpand-1 '(make-test-function test-values ((values 1 2 3)))))

(deftest test-make-test-function-return
         (assert-equal (funcall (make-test-function test-values ((+ 1 1)))) 2 ))

(deftest test-make-test-function-values
         (funcall (make-test-function test-values ((values 1 2 3)))))

(deftest test-make-test-function-values-call
         (funcall (make-test-function test-values ((return-values-fn 1 2 3)))))

(deftest test-make-test-function-run
         (assert-true (not (funcall (make-test-function test nil)))))

(deftest test-make-test-function-run-fail
         (assert-failure (funcall (make-test-function test ((assert-true nil))))))

(deftest test-run-test 
         (let ((result (make-instance 'unit-test::test-result)))
            (run-test (deftest hey (assert-true nil)) result)
            (assert-equal (length (slot-value result 'sub-results)) 1)
            (let ((sub-result (first (slot-value result 'sub-results))))
              (assert-true (typep sub-result 'unit-test::test-result))
              (assert-true (not (null (slot-value sub-result 'test-error))))
              (assert-true (typep (slot-value sub-result 'test-error) 'unit-test::unit-test-failure)))))

(deftest test-assert-equal-trim
         (assert-equal-trim "a" "a" "")
         (assert-equal-trim " a" "a" "")
         (assert-equal-trim "a" "a " "")
         (assert-equal-trim "a b c" "    a b c
                            " ""))

(deftest test-split-sequence 
         (assert-equal (split-sequence:split-sequence #\Space "a b c") '("a" "b" "c"))
         (assert-equal (split-sequence:split-sequence #\Newline "a
b
c") '("a" "b" "c")))

(deftest test-assert-equal-by-line-1
         (assert-equal-by-line "a
                               b
                               c" 
                               "a
                               b
                               c" "")
         (assert-failure (assert-equal-by-line "a" "z" "")))

(deftest test-assert-equal-by-line-2
         (assert-equal-by-line "a" "a" ""))
(deftest test-assert-equal-by-line-3
         (assert-failure (assert-equal-by-line "a" "a
                               b" "")))
(deftest test-assert-equal-by-line-4
         (assert-failure (assert-equal-by-line "a
                                               b" "a" "")))
(deftest test-assert-equal-by-line-5
         (assert-equal-by-line "a
                               b" "a
b" ""))
(deftest test-assert-equal-by-line-6
         (assert-equal-by-line "
                               a" "a" ""))
(deftest test-assert-equal-by-line-7
         (assert-equal-by-line "
                               a
                               b" "a
                               b" ""))
(deftest test-assert-equal-by-line-8
         (assert-equal-by-line "a" "
                               a" "1"))
(deftest test-assert-equal-by-line-9
         (assert-failure (assert-equal-by-line "a
                               b" "ab" "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :test-unit-test)

(let ((results (run-tests (lambda (test) 
                            (not (find (test-name test) '(test-fail-test)))))))
  (print results)
  (format t "~&Tests: ~a" (number-of-tests results)))


