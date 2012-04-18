
(load 'unit-test.lisp)
(use-package :unit-test)


(deftest test-mapcar
    (assert-equal (mapcar #'(lambda (x) (+ x x)) '(1 3 10)) '(2 6 20)))



(defun adder (c)
    #'(lambda (x) (+ x c)))

(deftest test-adder
    (assert-equal (mapcar (adder 3) '(1 3 10)) '(4 6 13))
    (assert-equal (mapcar (adder 10) '(1 3 10)) '(11 13 20)))

(defun bank-account (balance)
    #'(lambda (action amount)
        (case action
            (deposit (setf balance (+ balance amount)))
            (withdraw (setf balance (- balance amount)))))) 

(deftest test-lambda-closure
    (setf my-account (bank-account 500.00))
    (setf your-account (bank-account 250.00))
    (assert-equal (funcall my-account 'withdraw 75.00) 425.00)
    (assert-equal (funcall my-account 'withdraw 25.00) 400.00)
    (assert-equal (funcall your-account 'deposit 250.00) 500.00)
    (assert-equal (funcall your-account 'withdraw 100.00) 400.00))

(print (run-tests))


