
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)
(use-package :unit-test)

(deftest test-add
         (assert-equal (+ 3.14 2.71) 5.8500004 "" :test #'=))

(deftest test-friends
         (setf friends '(dick jane sally))
         (assert-equal friends '(dick jane sally))
         (setf enemies '(troll grinch ghost))
         (assert-equal enemies '(troll grinch ghost))
         (setf enemies (remove 'ghost enemies))
         (assert-equal enemies '(troll grinch))
         (setf friends (cons 'ghost friends))
         (assert-equal friends '(ghost dick jane sally))
         (defun newfriend (name)
           (setf enemies (remove name enemies))
           (setf friends (cons name friends)))
         (setf friends '(dick jane sally))
         (setf enemies '(troll grinch ghost))
         (newfriend 'ghost)
         (assert-equal friends '(ghost dick jane sally)))

(deftest test-prefix
         (assert-equal (* 9 3) 27)
         (assert-equal (/ 27 3) 9)
         (assert-equal (+ (* 2 2) (/ 2 2)) 5))

(deftest test-lists
         (assert-equal (first '(a b c)) 'a)
         (assert-equal (rest '(fast computers are nice)) '(computers are nice))
         (assert-equal (rest '(a b c)) '(b c))
         (assert-equal (rest '(c)) nil)
         (assert-equal (first ()) nil)
         (assert-equal (rest ()) nil)
         (assert-equal (first '((a b) (c d))) '(a b))
         (assert-equal (first (rest '(a b c))) 'b))


(deftest test-problem-2-2
         (assert-equal (first '(p h w)) 'p)
         (assert-equal (rest '(b k p h)) '(k p h))
         (assert-equal (first '((a b) (c d))) '(a b))
         (assert-equal (rest '((a b) (c d))) '((c d)))
         (assert-equal (first (rest '((a b) (c d)))) '(c d))
         (assert-equal (rest (first '((a b) (c d)))) '(b))
         (assert-equal (rest (first (rest '((a b) (c d))))) '(d))
         (assert-equal (first (rest (first '((a b) (c d))))) 'b))

(deftest test-problem-2-3
         (assert-equal (first (rest (first (rest '((a b) (c d) (e f)))))) 'd)
         (assert-equal (first (first (rest (rest '((a b) (c d) (e f)))))) 'e)
         (assert-equal (first (first (rest '(rest ((a b) (c d) (e f)))))) '(a b))
         (assert-error (first (first '(rest (rest ((a b) (c d) (e f)))))))
         (assert-equal (first '(first (rest (rest ((a b) (c d) (e f)))))) 'first)
         (assert-equal '(first (first (rest (rest ((a b) (c d) (e f)))))) 
         '(first (first (rest (rest ((a b) (c d) (e f))))))))

(deftest test-problem-2-4
         (assert-equal (first (rest (rest '(apple orange pear grapefruit)))) 'pear)
         (assert-equal (first (first (rest '((apple orange) (pear grapefruit))))) 'pear)
         (assert-equal (first (first (rest (rest '((apple) (orange) (pear) (grapefruit)))))) 'pear)
         (assert-equal (first (first (first (rest (rest '(apple (orange) ((pear)) (((grapefruit))))))))) 'pear)
         (assert-equal (first (first (rest (rest '((((apple))) ((orange)) (pear) grapefruit))))) 'pear)
         (assert-equal (first (rest (first '((((apple) orange) pear) grapefruit)))) 'pear))

(deftest test-setf
         (assert-equal (setf ab-list '(a b)) '(a b))
         (assert-equal ab-list '(a b))
         (assert-equal 'ab-list 'ab-list)
         (assert-equal (first ab-list) 'a)
         (assert-error (first 'ab-list))
         (assert-error (rest 'ab-list))
         (assert-equal (setf ab-list '(a b) xy-list '(x y)) '(x y))
         (assert-equal ab-list '(a b))
         (assert-equal xy-list '(x y)))

(deftest test-t-nil
         (assert-equal t 't)
         (assert-equal nil 'nil))

(deftest test-number
         (assert-equal 2 '2))

(deftest test-cons
         (assert-equal (cons 'a '(b c)) '(a b c))
         (setf new-front 'a old-list '(b c))
         (assert-equal (cons new-front old-list) '(a b c))
         (assert-equal new-front 'a)
         (assert-equal old-list '(b c))
         (assert-equal (first (cons new-front old-list)) 'a)
         (assert-equal (rest (cons new-front old-list)) '(b c)))

(deftest test-append
         (assert-equal (append '(a b c) '(x y z)) '(a b c x y z))
         (setf ab-list '(a b) xy-list '(x y))
         (assert-equal (append ab-list xy-list) '(a b x y))
         (assert-equal (append ab-list xy-list ab-list) '(a b x y a b))
         (assert-equal (append ab-list '()  xy-list '() ) '(a b x y ))
         (assert-error (append 'ab-list xy-list ))
         (assert-equal (append '((a) (b)) '((c) (d))) '((a) (b) (c) (d))))


(deftest test-list
         (setf front 'a middle 'b back 'c)
         (assert-equal (list front middle back) '(a b c))
         (assert-error (front middle back))
         (setf ab-list '(a b))
         (assert-equal (list ab-list ab-list) '((a b) (a b)))
         (assert-equal (list ab-list ab-list ab-list) '((a b) (a b) (a b)))
         (assert-equal (list 'ab-list ab-list) '(ab-list (a b))))

(deftest test-cons-append-list
         (setf ab-list '(a b) cd-list '(c d))
         (assert-equal (append ab-list cd-list) '(a b c d))
         (assert-equal (list ab-list cd-list) '((a b) (c d)))
         (assert-equal (cons ab-list cd-list) '((a b) c d))
         (assert-equal (append ab-list ab-list) '(a b a b))
         (assert-equal (list ab-list ab-list) '((a b) (a b)))
         (assert-equal (cons ab-list ab-list) '((a b) a b))
         (assert-error (append 'ab-list ab-list))
         (assert-equal (list 'ab-list ab-list) '(ab-list (a b)))
         (assert-equal (cons 'ab-list ab-list) '(ab-list a b)))

(deftest test-problem-2-5
         (assert-equal (append '(a b c) '( )) '(a b c))
         (assert-equal (list '(a b c) '()) '((a b c) ()))
         (assert-equal (cons '(a b c) '()) '((a b c))))

(deftest test-problem-2-6
         (setf tools (list 'hammer 'screwdriver))
         (assert-equal (cons 'pliers tools) '(pliers hammer screwdriver))
         (assert-equal tools '(hammer screwdriver))
         (setf tools (cons 'pliers tools))
         (assert-equal tools '(pliers hammer screwdriver))
         (assert-equal (append '(saw wrench) tools) '(saw wrench pliers hammer screwdriver))
         (assert-equal tools '(pliers hammer screwdriver))
         (setf tools (append '(saw wrench) tools))
         (assert-equal tools '(saw wrench pliers hammer screwdriver)))

(deftest test-problem-2-7
         (assert-equal (cons (first nil) (rest nil)) '(nil)))

(deftest test-cons-append-list-do-not-alter-symbol-values
         (setf new-front 'a old-list '(b c))
         (assert-equal (cons new-front old-list) '(a b c))
         (assert-equal new-front 'a)
         (assert-equal old-list '(b c)))

(deftest test-cons-template
         (setf new-front 'a list-to-be-changed '(b c))
         (setf list-to-be-changed (cons new-front list-to-be-changed))
         (assert-equal list-to-be-changed '(a b c))
         (assert-equal new-front 'a))

(deftest test-push-pop
         (setf new-front 'a list-to-be-changed '(b c))
         (push new-front list-to-be-changed )
         (assert-equal list-to-be-changed '(a b c))
         (assert-equal (pop list-to-be-changed)  'a)
         (assert-equal list-to-be-changed '(b c)))

(deftest test-nthcdr
         (setf abc-list '(a b c))
         (assert-equal (rest abc-list) '(b c))
         (assert-equal (nthcdr 2 abc-list) '(C))
         (assert-equal (nthcdr 50 abc-list) nil)
         (assert-equal abc-list '(a b c)))

(deftest test-butlast
         (setf abc-list '(a b c))
         (assert-equal (butlast abc-list 2) '(a))
         (assert-equal abc-list '(a b c)))

(deftest test-append-list
         (setf f 'front
               b 'back
               abc-list '(a b c))
         (assert-equal (cons f abc-list) '(front a b c))
         (assert-equal (append abc-list (list b)) '(a b c back)))

(deftest test-last 
         (setf abc-list '(a b c) ab-cd-list '((a b) (c d)))
         (assert-equal (last abc-list) '(c))
         (assert-equal (last ab-cd-list) '((c d)))
         ;;This is different from winson-lisp
         (assert-equal (last 'abc-list) 'abc-list)
         (assert-equal (first (last abc-list)) 'c)
         (assert-equal (car (last abc-list)) 'c))

(deftest test-length
         (setf ab-list '(a b) ab-cd-list '((a b) (c d)))
         (assert-equal (length ab-list) 2)
         (assert-equal (length ab-cd-list ) 2)
         (assert-equal (length (append ab-list ab-cd-list )) 4))

(deftest test-reverse
         (setf ab-list '(a b) ab-cd-list '((a b) (c d)))
         (assert-equal (reverse ab-list) '(b a))
         (assert-equal (reverse ab-cd-list) '((c d) (a b)))
         (assert-equal (reverse (append ab-list ab-list)) '(b a b a)))

(deftest test-problem-2-8
         (assert-equal (length '(plato socrates aristotle)) 3)
         (assert-equal (length '((plato) (socrates) (aristotle))) 3)
         (assert-equal (length '((plato socrates aristotle))) 1)
         (assert-equal (reverse '(plato socrates aristotle)) '(aristotle socrates plato))
         (assert-equal (reverse '((plato) (socrates) (aristotle))) '((aristotle) (socrates) (plato)))
         (assert-equal (reverse '((plato socrates aristotle))) '((plato socrates aristotle ))))

(deftest test-problem-2-9
         (assert-equal (length '((car chevrolet) (drink coke) (cereal wheaties))) 3)
         (assert-equal (reverse '((car chevrolet) (drink coke) (cereal wheaties))) '((cereal wheaties) (drink coke) (car chevrolet)))
         (assert-equal (append '((car chevrolet) (drink coke)) (reverse '((car chevrolet) (drink coke))))
                               '((car chevrolet) (drink coke) (drink coke) (car chevrolet))))
        

(deftest test-assoc
         (setf sarah '((height .54) (weight 4.4)))
         (assert-equal (assoc 'weight sarah) '(weight 4.4)))

(deftest test-numbers
         (assert-equal (/ 1.234321 1.111) 1.1110001)
         (assert-equal (/ 27 9) 3)
         (assert-equal (/ 22 7 ) 22/7)
         (assert-equal (float (/ 22 7)) 3.142857)
         (assert-equal (round (/ 22 7)) 3)
         (assert-equal (+ (round (/ 22 7)) (round (/ 7 3))) 5)
         (assert-equal (round (/ 5 2)) 2)
         (assert-equal (+ 2 1.5) 3.5)
         (assert-equal (+ (float 2) (float 1.5)) 3.5)
         (assert-equal (- 8) -8)
         (assert-equal (- -8) 8)
         (assert-equal (/ 1 2) 1/2)
         (assert-equal (/ 2) 1/2)
         (assert-equal (max 2 4 3) 4)
         (assert-equal (min 2 4 3) 2)
         (assert-equal (expt 2 3) 8)
         (assert-equal (expt 3 2) 9)
         (assert-equal (expt 3.3 2.2) 13.827086)
         (assert-equal (expt 2.2 3.3) 13.48947)
         (assert-equal (sqrt 9) 3)
         (assert-equal (expt 3 2) 9)
         (assert-equal (abs 5) 5)
         (assert-equal (abs -5) 5))

(deftest test-problem-2-10
         (assert-equal (/ (+ 3 1) (- 3 1)) 2)
         (assert-equal (* (max 3 4 5) (min 3 4 5)) 15)
         (assert-equal (min (max 3 1 4) (max 2 7 1)) 4))

(print (run-tests))
