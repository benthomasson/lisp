
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)
(use-package :unit-test)


(deftest test-both-ends
         (setf meals '(breakfast lunch tea dinner))
         (assert-equal (cons (first meals) (last meals)) '(breakfast dinner))
         (setf route2 '(boston cambridge lincoln concord))
         (assert-equal (cons (first route2) (last route2)) '(boston concord))
         (assert-equal (defun both-ends (whole-list)
           (cons (first whole-list) (last whole-list))) 'both-ends)
         (assert-equal (both-ends meals) '(breakfast dinner))
         (assert-equal (both-ends route2) '(boston concord)))

(deftest test-fences
         (test-both-ends)
         (setf whole-list '(monday tuesday wednesday thursday friday))
         (assert-equal (both-ends route2) '(boston concord))
         (assert-equal whole-list '(monday tuesday wednesday thursday friday)))

(deftest test-special-variable
         (test-fences)
         (defun both-ends-with-special-variable ()
           (setf whole-list (cons (first whole-list) (last whole-list))))
         (assert-equal (both-ends-with-special-variable) '(monday friday))
         (assert-equal whole-list '(monday friday)))

(deftest test-parameters
         (defun both-ends-with-two-parameters (l m)
           (cons (first l) (last m)))
         (assert-equal (both-ends-with-two-parameters '(breakfast lunch)
                                                      '(tea dinner))
                       '(breakfast dinner)))

(deftest test-side-effect
         (test-fences)
         (defun both-ends-with-side-effect (whole-list)
           (setf last-list-processed whole-list)
           (cons (first whole-list) (last whole-list)))
         (assert-equal (both-ends-with-side-effect whole-list) '(monday friday))
         (assert-equal last-list-processed whole-list))

(deftest test-problem-3-1
         (defun exchange (two-element-list)
           (cons (first (last two-element-list)) (list (first two-element-list))))
         (assert-equal (exchange '(adam eve)) '(eve adam))
         (defun exchange (two-element-list)
           (reverse two-element-list))
         (assert-equal (exchange '(adam eve)) '(eve adam))
         (defun exchange (two-element-list)
           (append (last two-element-list) (list (first two-element-list))))
         (assert-equal (exchange '(adam eve)) '(eve adam))
         (defun exchange (two-element-list)
           (list (elt two-element-list 1) (elt two-element-list 0)))
         (assert-equal (exchange '(adam eve)) '(eve adam)))

(deftest test-problem-3-2
         (defun construct (element a-list)
           (cons element a-list))
         (assert-equal (cons 'a '(b)) '(a b))
         (assert-equal (construct 'a '(b)) '(a b)))

(deftest test-problem-3-3
         (defun rotate-left (a-list)
           (append (rest a-list) (list (first a-list))))
         (assert-equal (rotate-left '(a b c)) '(b c a))
         (assert-equal (rotate-left (rotate-left '(a b c))) '(c a b)))

(deftest test-problem-3-4
         (defun rotate-right (a-list)
           (append (last a-list) (butlast a-list)))
         (assert-equal (rotate-right '(a b c)) '(c a b))
         (assert-equal (rotate-right (rotate-right '(a b c))) '(b c a)))

(deftest test-problem-3-5
         (defun palindromize (a-list)
           (append a-list (reverse a-list)))
         (assert-equal (palindromize '(a b)) '(a b b a))
         (assert-equal (palindromize '(a b c)) '(a b c c b a)))

(deftest test-problem-3-6
         (defun f-to-c (degrees)
           (- (* 5/9 (+ 40 degrees)) 40))
         (defun c-to-f (degrees)
           (- (* 9/5 (+ 40 degrees)) 40))
         (assert-equal (f-to-c 32) 0)
         (assert-equal (f-to-c 98.6) 37.000008)
         (assert-equal (f-to-c 212) 100)
         (assert-equal (c-to-f 0) 32)
         (assert-equal (c-to-f 37.0) 98.59999)
         (assert-equal (c-to-f 100) 212))

(deftest test-let
         (setf whole-list '(breakfast lunch tea dinner))
         (assert-equal (let ((element (first whole-list))
                             (trailer (last whole-list)))
                         (cons element trailer)) '(breakfast dinner))
         (defun both-ends-with-let (whole-list)
           (let ((element (first whole-list))
                 (trailer (last whole-list)))
             (cons element trailer)))
         (assert-equal (both-ends-with-let whole-list) '(breakfast dinner)))

(deftest test-let-in-parallel
         (setf x 'outside)
         (assert-equal (let ((x 'inside)
               (y x))
           (list x y)) '(inside outside)))

(deftest test-let*
         (setf x 'outside)
         (assert-equal (let* ((x 'inside)
                              (y x))
                         (list x y)) '(inside inside)))

(deftest test-let-let
         (setf x 'outside)
         (assert-equal (let ((x 'inside))
                         (let ((y x))
                           (list x y))) '(inside inside)))

(print (run-tests))

