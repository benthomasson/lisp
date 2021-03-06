
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)

(use-package :unit-test)


(let ((lst (list 1 2 3)))
  (let ((x (first lst))
        (y (second lst))
        (z (third lst)))
    (print-code
      x
      y
      z)
    (destructuring-bind (x y z) lst
      (print-code
        x
        y
        z))))


(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
    nil
    (let ((rest (cond ((funcall atom? pat) pat)
                      ((eq (car pat) '&rest) (cadr pat))
                      ((eq (car pat) '&body) (cadr pat))
                      (t nil))))
      (if rest
        `((,rest (subseq ,seq ,n)))
        (let ((p (car pat))
              (rec (destruc (cdr pat) seq atom? (1+ n))))
          (if (funcall atom? p)
            (cons `(,p (elt ,seq ,n))
                  rec)
            (let ((var (gensym)))
              (cons (cons `(,var (elt ,seq ,n))
                          (destruc p var atom?))
                    rec))))))))

 (defun dbind-ex (binds body)
   (if (null binds)
     `(progn ,@body)
     `(let ,(mapcar #'(lambda (b)
                        (if (consp (car b))
                          (car b)
                          b))
                    binds)
        ,(dbind-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                 (cdr b)))
                           binds)
                   body))))


(print-code
  (destruc '(a b c) 'seq #'atom)
  (destruc '(a (b . c) &rest d) 'seq)
  (dbind-ex (destruc '(a (b . c) &rest d) 'seq) '(body))
  (dbind (a b c) #( 1 2 3)
         (print-code a b c))
  (1+ (print 1))
  (print-code
    (print-code
      (print-code
        4)))
  nil
  ""
  (assert-error
    (dbind (a b c) (list 1 2)))
  (dbind (a (b c) d) '(1 #(2 3) 4)
         (print-code a b c d))
  (dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
         (print-code a b c d)))

(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan
                 #'(lambda (pat)
                     (incf row)
                     (setq col -1)
                     (mapcar #'(lambda (p)
                                 `(,p (aref ,gar
                                            ,row
                                            ,(incf col))))
                             pat))
                 pats))
         ,@body))))

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))


(print-code
  (setq ar (make-array '(3 3)))
  (for (r 0 2)
       (for (c 0 2)
            (setf (aref ar r c) (+ (* r 10) c))))
  ar
  (with-matrix ((a b c)
                (d e f)
                (g h i)) ar
               (print-code a b c d e f g h i))
  (with-array ((a 0 0) (d 1 1) (i 2 2)) ar
              (print-code a d i)))



(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplace-ex (destruc pat gseq #'atom) body))))

(defun wplace-ex (binds body)
  (if (null binds)
    `(progn ,@body)
    `(symbol-macrolet ,(mapcar #'(lambda (b)
                                   (if (consp (car b))
                                     (car b)
                                     b))
                               binds)
                      ,(wplace-ex (mapcan #'(lambda (b)
                                              (if (consp (car b))
                                                (cdr b)))
                                          binds)
                                  body))))

(print (with-places (a b c) #(1 2 3)
             (print (list a b c))))


(let ((lst '(1 (2 3) 4)))
  (print (with-places (a (b . c) d) lst
               (setf a 'uno)
               (setf c '(tre))))
               (print lst))


(defun match (x y &optional binds)
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
  (labels ((recbind (x binds)
                    (aif (assoc x binds)
                         (or (recbind (cdr it) binds)
                             it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(print-code
  (match '(p ?x b ?y a) '(p ?y b c a))
  (match '(a b c) '(a a a))
  (match '(a b c) '(a b c))
  (match '(a ?x b) '(_ 1 _))
  (match '(_ ?x _) '(a 1 b))
  (match '(_ 1 _) '(a ?x b)))


(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))


(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
    (if (var? expr) (list expr))
    (union (vars-in (car expr) atom?)
           (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))


(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
            (values ?x ?y)
            nil))

(print-code 
  (abab '(hi ho hi ho)))

(defmacro if-match (pat seq then &optional  else)
  `(let ,(mapcar #'(lambda (v) `(,v `,(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq, then ,else)))


(defmacro pat-match (pat seq then else)
  (if (simple? pat)
    (match1 `((,pat ,seq)) then else)
    (util:with-gensyms (gseq gelse)
                   `(labels ((,gelse () ,else))
                      ,(gen-match (cons (list gseq seq)
                                        (destruc pat gseq #'simple?))
                                  then
                                  `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
    then
    (let ((then (gen-match (cdr refs) then else)))
      (if (simple? (caar refs))
        (match1 refs then else)
        (gen-match (car refs) then else)))))


(print-code
  (destruc '(?x 'a) 'g #'simple?))


(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
         (cond ((gensym? pat)
                `(let ((,pat ,expr))
                   (if (and (typep ,pat 'sequence)
                            ,(length-test pat rest))
                     ,then
                     ,else)))
               ((eq pat '_) then)
               ((var? pat)
                (let ((ge (gensym)))
                  `(let ((,ge ,expr))
                     (if (or (gensym? ,pat) (equal ,pat ,ge))
                       (let ((,pat ,ge)) ,then)
                       ,else))))
               (t `(if (equal ,pat ,expr) ,then ,else)))))


(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
      `(= (length ,pat) ,(length rest))
      `(> (length ,pat) ,(- (length rest) 2)))))

(print-code (match1 '(((quote a) (elt g 1))) '(print ?x) nil))

(let ((n 3))
  (print-code
  (if-match (?x n 'n '(a b)) '(1 3 n (a b)) 
            ?x)))

(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
            (values ?x ?y)
            nil))

(print-code
  (abab "abab")
  (abab #(1 2 1 2))
  (if-match (?x (1 . ?y) . ?x) '((a b) #(1 2 3) a b)
            (values ?x ?y)))


(print (run-tests))



