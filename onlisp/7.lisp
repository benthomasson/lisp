
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)


(defmacro nil! (var)
  (list 'setq var nil))

(print-code
  (macroexpand-1 '(nil! a))
  (nil! a)
  a)

(print-code
  '(a b c)
  `(a b c)
  (setf a 1
        b 2
        c 3
        d 4)
    `(a b c)
    (list 'a 'b 'c)
    `(a ,b c ,d)
    (list 'a b 'c d)
    `(a ,b c)
    `(a (,b c))
    `(a b ,c (',(+ a b c)) (+ a b) 'c '((,a ,b))))

(defmacro nil! (var)
  `(setq ,var nil))

(print-code
  (macroexpand-1 '(nil! a))
  (nil! a)
  a)

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(print-code
  (macroexpand-1 '(nif x 'p 'z 'n))
  (mapcar #'(lambda (x)
              (nif x 'p 'z 'n))
          '(0 2.5 -8)))

(print-code 
  (setf b '(1 2 3))
  `(a ,b c)
  `(a ,@b c))

(defmacro our-when (test &body body)
  `(if ,test
     (progn
       ,@body)))

(print-code
  (macroexpand-1 '(our-when (eligible obj) (do-this) (do-that) obj)))



(defun greet (name)
  `(hello ,name))

(print-code 
  (greet 'ben))


(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(print-code 
  (macroexpand-1 '(memq 'a '(x y z a b c)))
  (memq 'a '(x y z a b c)))


(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(print-code
  (macroexpand '(while (able) (laugh)))
  (macroexpand-1 '(while (able) (laugh))))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(print-code
  (mac (while (able) (laugh))))

(defmacro mac-debug (expr)
  `(format t "~%~S -> ~S -> ~S" ',expr (macroexpand-1 ',expr) ,expr))

(print-code
  (mac-debug (memq 'a '(x y z a b c))))

(print-code 
      (destructuring-bind (x (y) . z) '(a (b) c d) (list x y z)))

    (defmacro our-dolist ((var list &optional result) &body body)
      `(progn
         (mapc #'(lambda (,var) ,@body)
               ,list)
         (let ((,var nil))
           ,result)))
               
    (print-code
      (our-dolist (x '(1 2 3) 1)
                  (print (1+ x))))

    (defmacro when-bind ((var expr) &body body)
      `(let ((,var ,expr))
         (when ,var
           ,@body)))

    (print-code
      (mac-debug (when-bind (x t) (print x)))
      (when-bind (x t)
                 (print x))
      (when-bind (x nil)
                 (print x)))


    (defmacro our-expander (name) `(get ,name 'expander))

    (defmacro our-defmacro (name parms &body body)
      (let ((g (gensym)))
        `(progn
           (setf (our-expander ',name)
#'(lambda (,g)
        (block ,name
               (destructuring-bind ,parms (cdr ,g)
             ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
    (funcall (our-expander (car expr)) expr)
    expr))

(print-code 
  (our-expander 'ed)
  (our-defmacro ed (x)
                `(print ,x))
  (our-expander 'ed)
  (our-macroexpand-1 '(ed 1)))

(let ((op 'setq))
  (defmacro our-setq (var val)
    (list op var val)))

(print-code 
  (our-setq x 1111)
  x)


(print-code 
  (let ((a 1))
    (setq a 2 b a)
    (list a b))
  (let ((a 1))
    (psetq a 2 b a)
    (list a b)))

(mac-debug (do ((w 3)
                (x 1 (1+ x))
                (y 2 (1+ y))
                (z))
             ((> x 10) (princ z) y)
             (princ x)
             (princ y)))


;;;Could not get the our-do macro on page 98 of On Lisp by Paul Graham to work in clisp
(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(block nil (let ,(make-initforms bindforms)
            (tagbody 
              ,label
            (if ,test
              (return (progn ,@result)))
            ,@body
            (psetq ,@(make-stepforms bindforms))
            (go ,label))))))

(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
              (if (consp b)
                (list (car b) (cadr b))
                (list b nil)))
          bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                (list (car b) (third b))
                nil))
          bindforms))

;;(trace our-do make-initforms make-stepforms)

(format t "~%~%")

(mac-debug (our-do ((w 3)
                        (x 1 (1+ x))
                        (y 2 (1+ y))
                        (z))
                       ((> x 10) (princ z) y)
                       (princ x)
                       (princ y)))

(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
          (our-and ,@(cdr args))))))

(defmacro our-andb (&rest args)
  (if (null args)
    t
    (labels ((expander (rest)
                       (if (cdr rest)
                         `(if ,(car rest)
                            ,(expander (cdr rest)))
                         (car rest))))
      (expander args))))

(print-code (macroexpand '(our-and a b c)))

(print-code (macroexpand '(mac-debug (our-and a b c))))

(let ((a 1)
      (b 2)
      (c 3))
(mac-debug (our-and a b c)))

(print-code 
  (our-and t t t)
  (our-and t t t nil) )

(print-code (macroexpand '(our-andb a b c)))

(let ((a 1)
      (b 2)
      (c 3))
(mac-debug (our-andb a b c)))

(print-code 
  (our-andb t t t)
  (our-andb t t t nil) )

(defmacro mac (x) `(1+ ,x))

(setq fn (compile nil '(lambda (y) (mac y))))

(defmacro mac (x) `(+ ,x 100))

(print-code (funcall fn 1))

(setq fn (compile nil '(lambda (y) (mac y))))

(print-code (funcall fn 1))

(print-code
  (symbol-macrolet ((hi (progn (print "Howdy")
                               1)))
                   (+ hi 2)))



(print (run-tests))
