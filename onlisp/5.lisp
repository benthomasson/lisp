
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(deftest test-complement
         (assert-true (funcall (complement #'oddp) 2))
         (assert-nil (funcall (complement #'oddp) 3)))

(defun joiner (obj)
  (typecase obj
    (cons #'append)
    (number #'+)
    (symbol #'list)))

(deftest test-joiner
         (assert-equal (joiner 1) #'+)
         (assert-equal (joiner '(a b c)) #'append))

(defun join (&rest args)
  (apply (joiner (car args)) args))

(deftest test-join
         (assert-equal (join 1 2 3 4) 10)
         (assert-equal (join 'a 'b 'c) '(a b c))
         (assert-equal (join '(a b) '(c d)) '(a b c d)))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(deftest test-make-adder
         (assert-equal (funcall (make-adder 3) 4) 7))

(defun our-complement (fn)
#'(lambda (&rest args) (not (apply fn args))))

(deftest test-our-complement
         (assert-true (funcall (our-complement #'oddp) 2))
         (assert-nil (funcall (our-complement #'oddp) 3)))

(defun funa (x) (+ x 2))
(defun func (x) (funcall #'funa x))

(deftest test-labels
         (labels ((funa (x) (+ x 1))
                  (funb (x) (funcall #'funa x)))
           (assert-equal (funa 1) 2)
           (assert-equal (funb 3) 4)
           ;;functions outside of the labels block are unaffected
           (assert-equal (func 3) 5)))

(deftest test-complement-2
         (assert-equal (remove-if (complement #'oddp) '(1 2 3 4 5 6)) '(1 3 5)))

(defvar *!equivs* (make-hash-table))

(defun !! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(deftest test-!
         (let ((*!equivs* (make-hash-table)))
           (def! #'remove-if #'delete-if)
           (assert-equal (!! #'remove-if) #'delete-if)
           (assert-equal (!! #'+) #'+))
           (assert-equal (!! #'remove-if) #'remove-if))


(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
            val
            (setf (gethash args cache) (apply fn args)))))))

(deftest test-memoize
         (setf slowid (memoize #'(lambda (x) (sleep 0.5) x)))
         (time (funcall slowid 1))
         (time (funcall slowid 1)))

(defun fibonacci (n)
  (if (or (zerop n) (= n 1))
    1
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(deftest test-fibonacci
         (assert-equal (fibonacci 0) 1)
         (assert-equal (fibonacci 1) 1)
         (assert-equal (fibonacci 2) 2)
         (assert-equal (fibonacci 3) 3)
         (assert-equal (fibonacci 4) 5))

(defun fibonacci-memo (n &optional (fn #'fibonacci-memo))
  (if (or (zerop n) (= n 1))
    1
    (+ (funcall fn (- n 1)) (funcall fn (- n 2)))))

(compile 'fibonacci)
(compile 'fibonacci-memo)

;;how bad is this???
;;Interesting use of closures and optional values
(setf (symbol-function 'fibonacci-memo) (memoize #'fibonacci-memo))

(compile 'fibonacci-memo)

(print #'fibonacci-memo)

(deftest test-fibonacci-memo
         (assert-equal (fibonacci-memo 0) 1)
         (assert-equal (fibonacci-memo 1) 1)
         (assert-equal (fibonacci-memo 2) 2)
         (assert-equal (fibonacci-memo 3) 3)
         (assert-equal (fibonacci-memo 4) 5)
         (dotimes (x 25)
           (assert-equal (print (fibonacci x)) (fibonacci-memo x)))
         (dotimes (x 1000)
           (print (fibonacci-memo x))))

(print 'fibonacci)
(time (print (fibonacci 25)))

(print 'fibonacci-memo)
(time (print (fibonacci-memo 25)))

(print (run-tests))
