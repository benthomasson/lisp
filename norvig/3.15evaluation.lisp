
(load 'unit-test.lisp)
(use-package :unit-test)

(deftest test-funcall-apply-eval
    (assert-equal (+ 1 2 3 4) 10)
    (assert-equal (funcall #'+ 1 2 3 4) 10)
    (assert-equal (apply #'+ '(1 2 3 4)) 10)
    (assert-equal (apply #'+ 1 2 '(3 4)) 10)
    (assert-equal (eval '(+ 1 2 3 4)) 10))

(print (run-tests))
