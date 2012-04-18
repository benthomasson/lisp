
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)

(use-package :unit-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match (x y &optional binds)
  (format t "~%match: x:~a y:~a binds:~a" x y binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
    ((binding x binds) (match it y binds))
    ((binding y binds) (match x y binds))
    ((varsym? x) (values (cons (cons x y) binds) t))
    ((varsym? y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (match (car x) (car y) binds))
     (match (cdr x) (cdr y) it))
    (t (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))


(defun binding (x binds)
  ;(format t "~%binding x:~a binds:~a" x binds)
  (labels ((recbind (x binds)
                    (aif (assoc x binds)
                         (or (recbind (cdr it) binds)
                             it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
    (if (var? expr) (list expr))
    (union (vars-in (car expr) atom?)
           (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (pred &rest args)
  `(progn
     (db-push ',pred ',args)
     ',args))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query #'atom))
         ,@body))))

(defun interpret-query (expr &optional binds)
  (format t "~%interpret-query expr:~a binds~a" expr binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t  (lookup (car expr) (cdr expr) binds))))

(defun interpret-and (clauses binds)
  (format t "~%interpret-and clauses:~a binds~a" clauses binds)
  (if (null clauses)
    (list binds)
    (mapcan #'(lambda (b)
                (interpret-query (car clauses) b))
            (interpret-and (cdr clauses) binds))))

(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
              (interpret-query c binds))
          clauses))

(defun interpret-not (clause binds)
  (if (interpret-query clause binds)
    nil
    (list binds)))

(defun lookup (pred args &optional binds)
  (mapcan #'(lambda (x)
              (aif2 (match x args binds) (list it)))
          (db-query pred)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

(print-code 
  (db-query 'painter)
  (db-query 'dates)
  (interpret-query '(painter hogarth ?x ?y)))

(print (macroexpand-1 '(with-answer (painter hogarth ?x ?y)
  (print (list ?x ?y)))))

(with-answer (painter hogarth ?x ?y)
  (print 'with-answer)
  (print (list ?x ?y)))

;(with-answer (and (painter ?x _ _)
;                  (dates ?x 1697 _))
;             (print (list ?x)))

(print (run-tests))
