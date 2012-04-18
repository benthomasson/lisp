
(load 'unit-test.lisp)
(use-package :unit-test)

;;3.2 special forms

(deftest test-defparameter
    (defparameter *a* 5)
    (defparameter *a* 6)
    (assert-equal *a* 6))

(deftest test-defvar
    (defvar *b* 5)
    (defvar *b* 6)
    (assert-equal *b* 5))

(deftest test-defconstant
    (defconstant *c* 5)
    ;;(defconstant *c* 6)
    (assert-equal *c* 5))


(defstruct name
    first
    (middle nil)
    last)


(deftest test-defstruct
         (setf me (make-name :first 'ben :last 'thomasson))
         (test-assert (name-p me))
         (assert-equal (name-first me) 'ben)
         (assert-equal (name-middle me) nil)
         (assert-equal (name-last me) 'thomasson)
         (setf (name-middle me) 'steven)
         (assert-equal (name-middle me) 'steven))

(deftest test-when
         (assert-equal (when t 1) 1)
         (assert-equal (when t (quote hey)) 'hey)
         (assert-equal (when nil 1) nil))

(deftest test-unless 
         (assert-equal (unless nil 1) 1)
         (assert-equal (unless t 1) nil))

(deftest test-cond
    (assert-equal (cond (nil 1)
          (t 2)
          (t 3)) 2 ))

(deftest test-and
         (assert-equal (and 'a 'b 'c) 'c)
         (assert-equal (and 'a nil 'c) nil))

(deftest test-or
         (assert-equal (or 'a 'b 'c) 'a))
         

(defun tax-bracket (income)
    (cond ((< income 10000.00) 0.00)
          ((< income 30000.00) 0.20)
          ((< income 50000.00) 0.25)
          ((< income 70000.00) 0.30)
          (t                   0.35)))

(deftest test-tax-bracket
    (assert-equal (tax-bracket 10) 0.00)
    (assert-equal (tax-bracket 59999) 0.30)
    (assert-equal (tax-bracket 99999) 0.35))

(deftest test-let
    (assert-equal (let ((x 40)
            (y (+ 1 1)))
        (+ x y)) 42))

(deftest test-lambda
    (assert-equal ((lambda (x y) (+ x y)) 45 (+ 3 1)) 49))

(deftest test-funcall
    (setf funct (lambda (x y) (+ x y)))
    (assert-equal (funcall funct 40 2) 42)
    (assert-equal (funcall funct 40 3) 43))

(deftest test-let*
    (assert-equal (let* ((x 6)
        (y (* x x)))
        (+ x y)) 42))

(print (run-tests))
