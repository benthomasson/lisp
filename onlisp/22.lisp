
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)

(defpackage onlisp-22
  (:use :cl :util))

(in-package :onlisp-22)

(defvar *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
    `(progn
       ,@(mapcar #'(lambda (c)
                     `(push #'(lambda () ,c) *paths*))
                 (reverse (cdr choices)))
       ,(car choices))
    `(fail)))


(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
    (progn
      (if (cdr choices)
        (push #'(lambda () (cb fn (cdr choices)))
              *paths*))
      (funcall fn (car choices)))
    (fail)))


(defun fail ()
  (if *paths*
    (funcall (pop *paths*))
    failsym))


(choose-bind x '(marrakesh strasbourg vegas)
             (format t "Let's go to ~A." x))

(print 'ready)


(=defun two-numbers ()
        (choose-bind n1 '(0 1 2 3 4 5)
                     (choose-bind n2 '(0 1 2 3 4 5)
                                  (=values n1 n2))))

(=defun parlor-trick (sum)
        (=bind (n1 n2) (two-numbers)
               (if (= (+ n1 n2) sum)
                 `(the sum of ,n1 ,n2)
                 (fail))))


(=defun descent (n1 n2)
        (declare (optimize (space 3)))
        (cond ((eq n1 n2) (=values (list n2)))
              ((kids n1) (choose-bind n (kids n1)
                                      (=bind (p) (descent n n2)
                                             (=values (cons n1 p)))))
              (t (fail))))

(defun kids (n)
  (case n
    (a '(b c))
    (b '(d e))
    (c '(d f))
    (f '(g))))

(compile 'kids)
(compile 'descent)
(compile '=descent)

(fail)
(fail)
(fail)
(fail)
(fail)
(fail)

(trace descent cb kids =descent fail choose-bind)

(trace util::=values)

(print util::*cont*)
(print (macroexpand '(descent 'a 'g)))
(print (descent 'a 'g))
