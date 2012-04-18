
(load 'unit-test.lisp)
(use-package :unit-test)

(deftest test-equal
    (setf a 5)
    (test-assert (equal a 5))
    (assert-equal a 5))

(deftest test-defvar
    (defvar b 6)
    (defvar b 7)
    (assert-equal b 6)
    (assert-not-equal b 7))

(deftest test-defparameter
    (defparameter c 6)
    (defparameter c 7)
    (assert-equal c 7)
    (assert-not-equal c 6))

(deftest test-defconstant
    (defconstant d 8)
    (assert-equal d 8))


(deftest test-defstruct
    (defstruct name
      first
      (middle nil)
      last)
    (setf me (make-name :first 'ben :last 'thomasson))
    (assert-equal (name-first me) 'ben)
    (assert-equal (name-middle me) nil)
    (assert-equal (name-last me) 'thomasson)
    (assert-predicate me #'name-p)
    (setf (name-middle me) 'steven)
    (assert-equal (name-middle me) 'steven))

(deftest test-elt
    (assert-equal (elt '(a b c) 0) 'a)
    (assert-equal (elt '(a b c) 1) 'b)
    (assert-equal (elt '(a b c) 2) 'c))

(deftest test-mapcar
    (assert-equal (mapcar #'+ '(1 2 3) '(1 2 3)) '(2 4 6)))

(print (run-tests))

