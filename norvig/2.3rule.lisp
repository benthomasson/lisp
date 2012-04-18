
(load 'unit-test.lisp)
(use-package :unit-test)

(defun mappend (fn the-list)
    (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
    (elt choices (random (length choices))))

(defparameter *simple-grammar* 
    '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
    "Some comment")

(defvar *grammar* *simple-grammar* 
    "Make grammar point to simple-grammar")

(print (assoc 'noun *grammar*))

(defun rule-lhs (rule)
    (first rule))

(deftest test-rule-lhs
         (assert-equal (rule-lhs '(noun-phrase -> (Article Noun))) 'noun-phrase))

(defun rule-rhs (rule)
    (rest (rest rule)))

(deftest test-rule-rhs
         (assert-equal (rule-rhs '(noun-phrase -> (Article Noun))) '((Article Noun))))

(defun rewrites (category)
    (rule-rhs (assoc category *grammar*)))

(deftest test-rewrites
         (assert-equal (rewrites 'Noun) '(man ball woman table))
         (assert-equal (rewrites 'Verb) '(hit took saw liked))
         (assert-equal (rewrites 'Article) '(the a))
         (assert-equal (rewrites 'verb-phrase) '((Verb noun-phrase)))
         (assert-equal (rewrites 'noun-phrase) '((Article Noun)))
         (assert-equal (rewrites 'sentence) '((noun-phrase verb-phrase)))
         (assert-equal (rewrites 'man) nil))

(print (rewrites 'noun))
(print (rewrites 'verb))

(deftest test-generate-parts
         (test-assert (listp (elt (rewrites 'sentence) 0)))
         (test-assert (atom (elt (rewrites 'Noun) 0)))
         (test-assert (null (rewrites 'man))))

(defun generate (phrase)
    (cond ((listp phrase)
        (mappend #'generate phrase))
    ((rewrites phrase)
        (generate (random-elt (rewrites phrase))))
    (t (list phrase))))

(deftest test-flet-random-elt
         (labels ((random-elt (choices)
                            (elt choices 0))
                (generate (phrase)
                    (cond ((listp phrase)
                        (mappend #'generate phrase))
                    ((rewrites phrase)
                        (generate (random-elt (rewrites phrase))))
                    (t (list phrase)))))
           (assert-equal (random-elt '(a b c d)) 'a)
           (assert-equal (random-elt '(a b c d)) 'a)
           (assert-equal (random-elt '(a b c d)) 'a)
           (assert-equal (random-elt '(a b c d)) 'a)
           (assert-equal (generate '(noun)) '(man))
           (assert-equal (generate '(noun)) '(man))
           (assert-equal (generate 'noun) '(man))
           (assert-equal (generate 'verb) '(hit))
           (assert-equal (generate 'article) '(the))
           (assert-equal (generate 'verb-phrase) '(hit the man))
           (assert-equal (generate 'noun-phrase) '(the man))
           (assert-equal (generate 'sentence) '(the man hit the man))))

;;(trace generate)

(print (generate 'sentence))

(deftest test-cond
    (assert-equal (cond (nil 1)
          (t 2)
          (t 3)) 2 ))

(print (generate 'noun-phrase))
(print (generate 'verb-phrase))
(print (generate 'noun))
(print (generate 'verb))
(print (generate 'article))

(defparameter *bigger-grammar*
    '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that))
    "Some comment")

 (setf *grammar* *bigger-grammar*)

 (print (generate 'sentence))
 (print (generate 'sentence))
 (print (generate 'sentence))
 (print (generate 'sentence))
 (print (generate 'sentence))

 (setf *grammar* *simple-grammar*)

(defun generate-tree (phrase)
    (cond ((listp phrase)
        (mapcar #'generate-tree phrase))
    ((rewrites phrase)
     (cons phrase
        (generate-tree (random-elt (rewrites phrase)))))
    (t (list phrase))))

(print (generate-tree 'sentence))
(print (generate-tree 'sentence))
(print (generate-tree 'sentence))

(defun generate-all (phrase)
    (cond ((null phrase) (list nil))
        ((listp phrase)
            (combine-all (generate-all (first phrase))
                         (generate-all (rest phrase))))
        ((rewrites phrase)
            (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
    (mappend #'(lambda (y)
        (mapcar #'(lambda (x) (append x y)) xlist))
    ylist))

(print (generate-all 'noun))
(print (generate-all 'article))
(print (generate-all 'verb))
(print (generate-all 'noun-phrase))
(print (generate-all 'sentence))

(print (run-tests))
