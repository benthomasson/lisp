
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun ret+ ()
#'+)

(print-code
  (lambda (x y) (+ x y))
  ((lambda (x y) (+ x y)) 1 2)
  (funcall (lambda (x y) (+ x y)) 1 2)
  (funcall #'(lambda (x y) (+ x y)) 1 2)
#'+
  (+ 1 1)
  (funcall (ret+) 1 1 ))

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x) (car x) x))
                     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))

(print-code
  (macroexpand-1 '(our-let ((a 1) (b 2)) (print a) (print b)))
  (let ((a 1) (b 2)) (print a) (print b))
  (our-let ((a 1) (b 2)) (print a) (print b)))


(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(let (,(car binds))
       (if ,(caar binds)
         (when-bind* ,(cdr binds) ,@body)))))

(defmacro our-with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))


(print-code 
  (when-bind* ((x (find-if #'consp '(a (1 2) b)))
               (y (find-if #'oddp x)))
              (+ y 10)))

(print-code
  (our-with-gensyms (gob x0 y0 x1 y1)
                    (print gob)
                    (print x0)
                    (print y0)
                    (print x1)
                    (print y1)))


(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                        (mapcar #'car
                                (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                      ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                (cons (cdr (assoc (car bindform) vars))
                      (cdr bindform))))
          (cdr cl)))

(print-code
  (macroexpand-1 '(condlet (((= 1 2) (x (print 'a)) (y (print 'b)))
            ((= 1 1) (y (print 'c)) (x (print 'd)))
            (t       (x (print 'e)) (z (print 'f))))
           (list x y z)))
  (condlet (((= 1 2) (x (print 'a)) (y (print 'b)))
            ((= 1 1) (y (print 'c)) (x (print 'd)))
            (t       (x (print 'e)) (z (print 'f))))
           (list x y z)))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))


(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))


(print-code
  (if t 'phew (/ x 0))
  (mapcar #'(lambda (x)
              (nif x 'p 'z 'n))
          '(0 1 -1)))


(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c)) choices)))))

(print-code 
  (in 'a 'a 'b 'c 'e)
  (in 5 (+ 1 2) (+ 2 3) (+ 4 5))
  (macroexpand-1 '(in 5 (+ 1 2) (+ 2 3) (+ 4 5))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(print-code
  (let ((operator '+))
    (inq operator + - *))
  (macroexpand-1 '(inq operator + - *)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(print-code
  (let ((a 2)
        (b 9))
  (in-if #'oddp a b))
  (macroexpand-1 '(in-if #'oddp a b)))


(defmacro >case (expr &rest clauses)
  (let ((fnsym (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl) (rest cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))


(print-code
  '???)

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
     (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
       ((> ,var ,gstop))
       ,@body)))

(print-code
  (for (x 1 5)
       (print x)))

(defmacro forever (&body body)
  `(block nil
          (do ()
            (nil)
            ,@body)))

(print-code 
  (let ((loop 0))
  (forever 
    (if (= loop 5)
      (return loop)
      (incf loop)))))
           

(print (run-tests))
