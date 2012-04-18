
(defpackage :winston24
  (:use :cl :util :unit-test)
  (:export :? 
	   :fail
	   :add-binding
	   :extract-variable
	   :make-binding
	   :find-binding
	   :extract-key
	   :extract-value
	   :match-atoms
	   :match-variable
	   :match-pieces
	   :elements-p
	   :variable-p
	   :match
	   :unify))

(in-package :winston24)

(defun add-binding (pattern-variable-expression datum bindings)
  (if (eq `_ (extract-variable pattern-variable-expression))
    bindings
    (cons (make-binding
            (extract-variable pattern-variable-expression)
            datum)
          bindings)))

(defun extract-variable (pattern-variable-expression)
  (second pattern-variable-expression))

(defun make-binding (variable datum)
  (list variable datum))

(defun find-binding (pattern-variable-expression binding)
  (unless (eq '_ (extract-variable pattern-variable-expression))
    (assoc (extract-variable pattern-variable-expression) binding)))

(defun extract-key (binding)
  (first binding))

(defun extract-value (binding)
  (second binding))


(defun match-atoms (p d bindings)
  (if (eql p d)
    bindings
    'fail))

(defun match-variable (p d bindings)
  (let ((binding (find-binding p bindings)))
    (if binding
      (match (extract-value binding) d bindings)
      (add-binding p d bindings))))

(defun match-pieces (p d bindings)
  (let ((result (match (first p) (first d) bindings)))
    (if (eq 'fail result)
      'fail
      (match (rest p) (rest d) result))))

(defun elements-p (p d) 
  (and (atom p) (atom d)))

(defun variable-p (p)
  (and (listp p) (eq '? (first p))))

(defun recursive-p (p d)
  (and (listp p) (listp d)))

(defun match (p d &optional bindings)
  (cond 
    ((elements-p p d) (match-atoms p d bindings))
    ((variable-p p) (match-variable p d bindings))
    ((recursive-p p d) (match-pieces p d bindings))
    (t 'fail)))

(defun unify-atoms (p1 p2 bindings)
  (if (eql p1 p2) bindings 'fail))

(defun unify-pieces (p1 p2 bindings)
  (let ((result (unify (first p1) (first p2) bindings)))
    (if (eq 'fail result)
      'fail
      (unify (rest p1) (rest p2) result))))

(defun unify-variable (p1 p2 bindings)
  (let ((binding (find-binding p1 bindings)))
    (if binding
      (unify (extract-value binding) p2 bindings)
      (if (insidep p1 p2 bindings)
        'fail
        (add-binding p1 p2 bindings)))))

(defun insidep (variable expression bindings)
  (if (equal variable expression)
    nil
    (inside-or-equal-p variable expression bindings)))

(defun inside-or-equal-p (variable expression bindings)
  (cond ((equal variable expression) t)
        ((atom expression) nil)
        ((eq '? (first expression))
         (let ((binding (find-binding expression bindings)))
           (when binding
             (inside-or-equal-p variable
                                (extract-value binding)
                                bindings))))
        (t (or (inside-or-equal-p variable
                                  (first expression)
                                  bindings)
               (inside-or-equal-p variable
                                  (rest expression)
                                  bindings)))))

(defun unify (p1 p2 &optional bindings)
  (cond ((elements-p p1 p2)
         (unify-atoms p1 p2 bindings))
        ((variable-p p1)
         (unify-variable p1 p2 bindings))
        ((variable-p p2)
         (unify-variable p2 p1 bindings))
        ((recursive-p p1 p2)
         (unify-pieces p1 p2 bindings))
        (t 'fail)))


(deftest test-add-binding
         (assert-equal '((x apple) (y red)) (add-binding '(? x) 'apple '((y red))))
         (assert-equal '((y red)) (add-binding '(? _) 'apple '((y red)))))

(deftest test-find-binding
         (assert-equal '(x apple) (find-binding '(? x) '((x apple) (y red))))
         (assert-equal '(y red) (find-binding '(? y) '((x apple) (y red))))
         (assert-nil (find-binding '(? _) '((x apple) (y red)))))

(deftest test-match-1
         (assert-nil (match 'a 'a))
         (assert-equal 'fail (match 'a 'b))
         (assert-equal '((x apple)) 
                       (match '((? x)) '(apple)))
         (assert-nil (match '((? _)) '(apple)))
         (assert-equal '((y red) (x apple)) 
                       (match '(color (? x) (? y)) '(color apple red))))


(deftest test-unify
         (assert-equal '((y red) (x apple)) 
                       (unify '(color (? x) (? y)) '(color apple red)))
         (assert-equal '((m red) (l apple))
                       (unify '(color apple red) '(color (? l) (? m))))
         (assert-equal '((y (? m)) (x (? l)))
                       (unify '(color (? x) (? y)) '(color (? l) (? m))))
         (assert-equal '((y (? y)) (x (? x)))
                       (unify '(color (? x) (? y)) '(color (? x) (? y))))
         (assert-equal '((x (patrick is-a person)))
                       (unify '((? x) with (hair blond))
                              '((patrick is-a person) with (hair blond))))
         (assert-equal '((x (patrick is-a (? y))))
                       (unify '((? x) with (hair blond))
                              '((patrick is-a (? y)) with (hair blond))))
         (assert-equal 'fail
                       (unify '((? x) with (hair blond))
                              '((patrick is-a (? x)) with (hair blond)))))


(print (run-tests))


