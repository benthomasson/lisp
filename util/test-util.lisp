
(shadow :with-gensyms)

(use-package :util)

(deftest test-mklist
         (assert-equal (mklist 'a) '(a))
         (assert-equal (mklist '(a)) '(a)))

(deftest test-single
         (assert-true (single '(a)))
         (assert-nil (single '()))
         (assert-nil (single '(a b))))

(deftest test-double
         (assert-true (double '(a b)))
         (assert-nil (double '()))
         (assert-nil (double '(a b c)))
         (assert-nil (double '(a))))

(deftest test-triple
         (assert-true (triple '(a b c)))
         (assert-nil (triple '()))
         (assert-nil (triple '(a b c d)))
         (assert-nil (triple '(a b)))
         (assert-nil (triple '(a))))

(deftest test-quadruple
         (assert-true (quadruple '(a b c d)))
         (assert-nil (quadruple '()))
         (assert-nil (quadruple '(a b c d e)))
         (assert-nil (quadruple '(a b c)))
         (assert-nil (quadruple '(a))))

(mac-debug (print-code t))
(mac-debug (print-code (error "a")))

(print (run-tests))

