;
(defpackage :5eliza
  (:use :cl :util :unit-test))

(in-package :5eliza)

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun simple-equal (x y)
  "Are x and y equal? (Don't chek inside strings.)?"
  (if (or (atom x) (atom y))
    (eql x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest y)))))

(deftest test-simple-equal
           (assert-true (simple-equal 'a 'a))
           (assert-true (not (simple-equal 'a 'b)))
           (assert-true (simple-equal '(1 2 3) '(1 2 3)))
           (assert-true (not (simple-equal '(1 2 3) '(1 2 3 4))))
           (assert-true (not (simple-equal '(1 2 3 4) '(1 2 3))))
           (assert-true (not (simple-equal '(a b c) '(1 2 3)))))

;(print-code (sublis '((?X . vacation)) '(what would it mean if you got a ?X ?)))

(#+ :ignore (defun pat-match (pattern input)
  "Does pattern match input? Any variable can match anything."
  (if (variable-p pattern)
    t
    (if (or (atom pattern) (atom input))
      (eql pattern input)
      (and (pat-match (first pattern) (first input))
           (pat-match (rest pattern) (rest input)))))))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(deftest test-variable-p
           (assert-true (variable-p '?X))
           (assert-true (not (variable-p 'X)))
           (assert-true (variable-p '?Y)))

(deftest test-pat-match
           (assert-true (pat-match '(I need a ?X) '(I need a vacation)))
           (assert-true (pat-match '(I need a) '(I need a)))
           (assert-true (pat-match '(?X ?Y ?Z ?W) '(I need a vacation))))

           
(deftest test-sublis
           (assert-equal (sublis '((?X . vacation)) '(I need a ?X)) '(I need a vacation))
           (assert-equal (sublis '((?X . a)) '(a ?X a ?X)) '(a a a a)))

(deftest test-car-cdr
           (assert-equal (car '(a . b)) 'a)
           (assert-equal (cdr '(a . b)) 'b))

(deftest test-cons
           (assert-equal (cons 'a 'b) '(a . b))
           (assert-equal (car (cons 'a 'b)) 'a)
           (assert-equal (cdr (cons 'a 'b)) 'b))

(defconstant fail nil )

(defconstant no-bindings '((t . t)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the values part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(#+ :ignore (defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val) bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pari to a binding list."
  (cons (cons var val)
        ;;Once we add a "real" binding,
        ;;we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
          nil
          bindings)))

(deftest test-extend-bindings
;;(print-code (extend-bindings 'a 'b no-bindings))
           (assert-equal (extend-bindings 'a 'b no-bindings) '((a . b)))
           (assert-equal (extend-bindings 'a 'b nil) '((a . b))))

(deftest test-binding-val-get-binding-lookup
           (assert-equal (binding-val (cons 'a 'b)) 'b)
           (assert-equal (get-binding 'a '((a . b))) '(a . b) )
           (assert-equal (lookup 'a '((a . b))) 'b ))

(#+ :ignore (defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings."
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail))))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(deftest test-match-variable
;;(print-code (match-variable '?X '(a) nil))
           (assert-equal (match-variable '?X '(a) nil) '((?X a)))
           (assert-equal (match-variable '?X '(a b c) nil) '((?X a b c)))
           (assert-equal (match-variable '?X '(a b c) no-bindings) '((?X a b c))))


(deftest test-pat-match-2
;;(print-code (pat-match '(i need a ?X) '(i need a vacation)))
 (assert-equal (pat-match '(i need a vacation) '(i need a vacation)) no-bindings)
 (assert-equal (pat-match '(i really need a ?X) '(i need a vacation)) nil )
 (assert-equal (pat-match '(?X is ?X) '(2 is 2)) '((?X . 2) ))
 (assert-equal (pat-match '(?X is ?Y) '(2 is 3)) '((?y . 3) (?x . 2)))
 (assert-equal (pat-match '(i need a ?X) '(i need a vacation)) '((?X . vacation) )))
 
;;(print-code 
;;  (sublis (pat-match '(i need a ?X) '(i need a vacation))
;;          '(what would it mean to you if you got a ?X ?)))


(defun pat-match (pattern input &optional (bindings no-bindings))
  "Math pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input ) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      ;;We assume that pat starts with a constant
      ;;In other words, a pattern can't have 2 consecutive vars
      (let ((pos (position (first pat) input
                           :start start :test #'equal)))
        (if (null pos)
          fail
          (let ((b2 (pat-match pat (subseq input pos) bindings)))
            ;;If this match failed, try another longer one
            ;; If it worked check that the variables match
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              (match-variable var (subseq input 0 pos) b2))))))))

;(print-code (pat-match '((?* ?p) need (?* ?x))
;                       '(Mr Hurlot and I need a vacation)))

;(print-code (pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool)))

;(print-code (pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b)))

(#+ :ignore (defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      ;;We assume that pat starts with a constant
      ;;In other words, a pattern can't have 2 consecutive vars
      (let ((pos (position (first pat) input
                           :start start :test #'equal)))
        (if (null pos)
          fail
          (let ((b2 (pat-match
                      pat (subseq input pos)
                      (match-variable var (subseq input 0 pos)
                                      bindings))))
            ;;If this match failed, try another longer one
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              b2))))))))

;(print-code (pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b)))

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do  you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really== if ?y))
    (((?* ?x) no (?* y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Where you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                (sublis (switch-viewpoint result)
                        (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are)) words))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))


;;(eliza)

(print (run-tests))
