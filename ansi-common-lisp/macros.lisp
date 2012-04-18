
(load 'unit-test.lisp)
(use-package :unit-test)

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
       ((> ,var ,gstop))
       ,@body)))

(print (macroexpand-1 '(for x 0 10 (print x ) (push x lst))))

(deftest test-for
         (setf a -1)
         (for x 0 10
              (setf a x))
         (assert-equal a 10))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c)) choices)))))

(print (macroexpand-1 '(in 'a 'a 'b 'c 'd 'e)))

(deftest test-in
         (assert-true (in 'a 'a))
         (assert-nil (in 'a 'b))
         (assert-true (in 'a 'b 'c 'a)))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(print (macroexpand-1 '(random-choice 'a 'b 'c 'd 'e)))
                 
(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(print (macroexpand-1 '(avg 1 2 3 4 5)))

(deftest test-avg
         (assert-equal (avg 1 2 3 4 5) 3))

(defmacro with-gensyms (sysms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 sysms)
     ,@body))

(print (macroexpand-1 '(with-gensyms (a b c d) (+ a b c d))))

(defmacro aif (test then &optional else )
  `(let ((it ,test))
     (if it ,then ,else)))

(print (macroexpand-1 '(aif (+ 1 1)
                            (1+ it)
                            0)))

(deftest test-aif 
         (assert-equal (aif (+ 1 1)
                            (1+ it)
                            0) 3)
         (assert-equal (aif nil
                            (1+ it)
                            it) nil))


(defmacro nil! (x)
  `(setf ,x nil))

(print (macroexpand-1 '(nil! xyz)))

(deftest test-nil!
         (setf xyz 4)
         (setf abc 'abc)
         (assert-nil (nil! xyz))
         (assert-nil xyz)
         (assert-nil (nil! abc))
         (assert-nil abc))

(defmacro while (test &rest body)
  `(do ()
     ((not ,test))
     ,@body))

(print (macroexpand-1 '(while (> value 0) (decf value))))

(deftest test-while
         (setf value 10)
         (assert-equal value 10)
         (while (> value 0)
                (decf value))
         (assert-equal value 0))

(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
         ((>= ,g ,h))
         ,@body))))

(print (macroexpand-1 '(ntimes (setf v (- v 1)) (princ "."))))

(defmacro ntimes (n &rest body)
  (with-gensyms (g h)
                `(let ((,h ,n))
                   (do ((,g 0 (+ ,g 1)))
                     ((>= ,g ,h))
                     ,@body))))

(print (macroexpand-1 '(ntimes (setf v (- v 1)) (princ "."))))
               

(print (run-tests))

