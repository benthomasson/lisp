

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun match (x y &optional binds)
  (cond
    ((eql x y) (values binds t))
    ((assoc x binds) (match (binding x binds) y binds))
    ((assoc y binds) (match x (binding y binds) binds))
    ((var? x ) (values (cons (cons x y) binds) t))
    ((var? y ) (values (cons (cons y x) binds) t))
    (t 
      (when (and (consp x) (consp y))
        (multiple-value-bind (b2 yes)
          (match (car x) (car y) binds)
          (and yes (match (cdr x) (cdr y) b2)))))))

(defun var? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b 
      (or (binding (cdr b) binds)
          (cdr b)))))

(deftest test-var?
         (assert-nil (var? 1))
         (assert-nil (var? t))
         (assert-nil (var? 'x))
         (assert-true (var? '?x)))

(deftest test-binding
         (assert-nil (binding '?x nil))
         (assert-equal (binding '?x '((?x . 4))) 4 )
         (assert-equal (binding '?x '((?x . ?y))) '?y)
         (assert-equal (binding '?x '((?x . ?y) (?y . 5))) 5 ))

(deftest test-match
         (multiple-value-bind (binding does-match) (match nil nil)
           (assert-nil binding)
           (assert-true does-match))
         (assert-nil (match '(a b c) '(a b x)))
         (assert-nil (match '(a b c) '(a b c)))
         (multiple-value-bind (binding yes) (match '(p a b c a) '(p ?x ?y c ?x))
           (assert-true yes)
           (assert-equal binding '((?y . b) (?x . a))))
         (multiple-value-bind (binding yes) (match '(?x a) '(?y ?y))
           (assert-true yes)
           (assert-equal binding '((?y . a) (?x . ?y))))
         (assert-nil (match '(a) '(a)))
         (assert-equal (match '(?x) '(a)) '((?x . a)))
         (assert-equal (match '(a) '(?x)) '((?x . a))))

(defvar *rules* (make-hash-table))

(defmacro <- (con &optional ant)
  `(length (push (cons (cdr ',con) ',ant)
                 (gethash (car ',con) *rules*))))

(deftest test-add-rule
         (let ((*rules* (make-hash-table)))
           (<- (a b c))
           (<- (a b c) (d e f))
           (<- (x y z) (d e f))
           (assert-equal (gethash 'a *rules*) '(((B C) D E F) ((B C))))
           (assert-equal (gethash 'x *rules*) '(((Y Z) D E F)))))

(defun prove (expr &optional binds)
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or  (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t   (prove-simple (car expr) (cdr expr) binds))))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
              (multiple-value-bind (b2 yes)
                (match args (car r) binds)
                (when yes
                  (if (cdr r)
                    (prove (cdr r) b2)
                    (list b2)))))
          (mapcar #'change-vars
                  (gethash pred *rules*))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")))
                  (vars-in r))
          r))

(defun vars-in (expr)
  (if (atom expr)
    (if (var? expr) (list expr))
    (union (vars-in (car expr))
           (vars-in (cdr expr)))))

(deftest test-prove-simple
         (let ((*rules* (make-hash-table)))
         (<- (parent donald nancy))
         (<- (child ?x ?y) (parent ?y ?x))
         (assert-equal (gethash 'parent *rules*) '(((donald nancy))))
         (assert-equal (gethash 'child *rules*) '(((?x ?y) parent ?y ?x)))
         (assert-equal (prove-simple 'parent '(donald nancy) nil) '(nil))
         (assert-equal (length (first (prove-simple 'child '(?x ?y) nil))) 4))) 

(defun prove-and (clauses binds)
  (if (null clauses)
    (list binds)
    (mapcan #'(lambda (b)
                (prove (car clauses) b))
            (prove-and (cdr clauses) binds))))

(defun prove-or (clauses binds)
  (mapcan #'(lambda (c) (prove c binds))
          clauses))

(defun prove-not (clause binds)
  (unless (prove clause binds)
    (list binds)))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query))
         ,@body))))

(deftest test-vars-in
         (assert-nil (vars-in '(a b c)))
         (assert-equal (vars-in '(?x ?y ?z)) '(?x ?y ?z))
         (assert-equal (vars-in '(a b c ?z)) '(?z))
         (assert-equal (vars-in '(?x 1 2 3 ?y a b c ?z)) '(?x ?y ?z))
         (assert-equal (vars-in '((((?x))) 1 2 3 (?y a b c) ?z)) '(?x ?y ?z))
         (assert-equal (vars-in '(?x)) '(?x)))

(deftest test-change-vars
         (assert-equal (change-vars '(a b c)) '(a b c))
         (assert-not-equal (change-vars '(((?x)) b c)) '(((?x)) b c))
         (assert-not-equal (change-vars '(?x b c)) '(?x b c)))

(print (macroexpand-1 '(with-answer (p ?x ?y)
                    (f ?x ?y))))

(<- (parent donald nancy))
(<- (parent donald debbie))
(<- (male donald))
(<- (father ?x ?y) (and (parent ?x ?y) (male ?x)))
(<- (= ?x ?x))
(<- (sibling ?x ?y) (and (parent ?z ?x)
                         (parent ?z ?y)
                         (not (= ?x ?y))))

(with-answer (parent ?x ?y)
             (format t "~%~A is the parent of ~A." ?x ?y))

(with-answer (sibling ?x ?y)
             (format t "~%~A is the sibling of ~A." ?x ?y))

(print (run-tests))
