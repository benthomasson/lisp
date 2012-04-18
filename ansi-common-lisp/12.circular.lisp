
(load "../util/util.lisp")
(use-package :util)

(setf *print-circle* t)

(print-code
  (setf x (list 'a))
  (progn (setf (cdr x) x) nil)
  x
  (defun circular (lst)
    (setf (cdr (last lst)) lst))
  (circular (list 'a 'b 'c 'd))
  (let ((y (list 'a)))
    (setf (car y) y)
    y)

  )
