
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)
(use-package :unit-test)

(deftest test-predicate
         (assert-equal t t)
         (assert-equal 't t)
         (assert-equal nil nil)
         (assert-equal nil 'nil)
         (assert-equal nil '())
         (assert-equal nil ())
         (assert-true (equal (+ 2 2) 4))
         (assert-nil (equal (+ 2 3) 3))
         (assert-true (equal '(this is a list) (setf l '(this is a list))))
         (assert-true (equal '(this is a list) l))
         (assert-nil (equal '(this is a list) (setf reverse-of-l '(list a is this))))
         (assert-true (equal l (reverse reverse-of-l))))

(deftest test-equal-eql-eq-=
         (assert-true (eq 4 4))
         (assert-nil (eql 4 4.0))
         (assert-true (eql 4 4))
         (assert-true (- 4 4.0)))

(deftest test-member
         (setf sentence '(tell me more about your mother please))
         (assert-equal (member 'mother sentence) '(mother please))
         (assert-true (member 'mother sentence))
         (setf pairs '((father son) (mother daugther)))
         (assert-nil (member 'mother pairs)))

(deftest test-member-test-equal
         (setf pairs '((maple shade) (apple fruit)))
         (assert-nil (member '(maple shade) pairs))
         (assert-true (member '(maple shade) pairs :test #'equal))
         (assert-equal :test :test)
         (setf predicate #'equal)
         (assert-true (member '(maple shade) pairs :test predicate))
         (assert-equal (member '(maple shade) pairs :test-not #'equal) '((apple fruit))))


(deftest test-listp-atom-symbolp
         (assert-true (atom 'pi))
         (assert-true (atom pi))
         (assert-nil (numberp 'pi))
         (assert-true (numberp pi))
         (assert-true (symbolp 'pi))
         (assert-nil (symbolp pi))
         (assert-nil (listp pi))
         (assert-nil (listp 'pi))
         (assert-true (listp '(this is a list with pi in it))))

(deftest test-nil
         (assert-true (eq nil '()))
         (assert-true (eql nil '()))
         (assert-true (equal nil '()))
         (assert-true (atom nil))
         (assert-true (atom ()))
         (assert-true (symbolp nil))
         (assert-true (symbolp ()))
         (assert-true (listp nil))
         (assert-true (listp ()))
         (assert-nil (consp nil))
         (assert-nil (consp ()))
         (assert-true (null nil))
         (assert-true (null ()))
         (assert-true (endp nil))
         (assert-true (endp ()))
         (assert-nil (null 'a-symbol))
         (assert-error (endp 'a-symbol)))

(deftest test-done
         (test-fail "not done"))

(print (run-tests))
