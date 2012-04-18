
(load 'unit-test.lisp)
(use-package :unit-test)

(defun simple-equal (x y)
  "Test if two lists or atoms are equal."
  ;;Warning - incorrect
  (or (eql x y)
      (and (listp x) (listp y)
           (simple-equal (first x) (first y))
           (simple-equal (rest x) (rest y)))))

(defun simple-equal (x y)
  "Are x and y equal? (Don't chek inside strings.)?"
  (if (or (atom x) (atom y))
    (eql x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest y)))))

(deftest test-simple-equal
           (test-assert (simple-equal 'a 'a) 1)
           (test-assert (simple-equal '(a) '(a)) 2)
           (test-assert (simple-equal '(a b c) '(a b c)) 3)
           (test-assert (simple-equal nil nil) 4)
           (test-assert (simple-equal '() nil) 5)
           (test-assert (simple-equal nil '()) 6)
           (test-assert (not (simple-equal '(a) 'a)) 9)
           (test-assert (not (simple-equal '(2) 2)) 10)
           (test-assert (not (simple-equal '(a b c) '(a b))) 11)
           (test-assert (not (simple-equal '(a b) '(a b c))) 12)
           (test-assert (not (simple-equal 'a 'b)) 13)
           (test-assert (simple-equal '() '(nil . nil)) 7)
           (test-assert (simple-equal nil '(nil . nil)) 8))


(print (run-tests))
