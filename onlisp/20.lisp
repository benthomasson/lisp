
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)

(defpackage onlisp-20
  (:use :cl :util))

(in-package :onlisp-20)

(defun all-identity (&rest args)
  (values-list args))

(defvar *cont* #'all-identity)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(, ',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (an-fn &rest args)
  `(funcall ,an-fn *cont* ,@args))

(defmacro =apply (an-fn &rest args)
  `(apply ,an-fn *cont* ,@args))


(print (macroexpand-1 '(=defun add1 (x) (=values (1+ x)))))
(print (macroexpand-1 '(=values (1+ n))))


(=defun add1 (x) 
        (=values (1+ x)))

(print (add1 5))

(=defun bar (x)
        (=values (list 'a (add1 x))))

(print (bar 5))

(=defun message ()
        (=values 'hello 'there))

(print-code (message))

(print (macroexpand-1 '(=defun message () (=values 'hello 'there))))

(=defun baz ()
        (=bind (m n) (message)
               (=values (list m n))))

(print (macroexpand-1 '(=defun baz () (=bind (m n) (message) (=values (list m n))))))

(print (baz))

(defun dft (tree)
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
           (dft (cdr tree)))))

(defvar *saved* nil)

(=defun dft-node (tree)
        (cond ((null tree) (my-restart))
              ((atom tree) (=values tree))
              (t (push #'(lambda () (dft-node (cdr tree)))
                       *saved*)
                 (dft-node (car tree)))))

(=defun my-restart ()
        (if *saved*
          (funcall (pop *saved*))
          (=values 'done)))



(=defun dft2 (tree)
        (setq *saved* nil)
        (=bind (node) (dft-node tree)
               (cond ((eq node 'done) (=values nil))
                     (t (princ node)
                        (my-restart)))))

(setq t1 '(a (b (d h)) (c e (f i) g))
      t2 '(1 (2 (3 6 7) 4 5)))

;(trace my-restart dft2 =dft2)

(fresh-line)
(dft t1)
(fresh-line)
(dft2 t1)


