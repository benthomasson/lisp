
(load "../util/util.lisp")
(use-package :util)

(print-code 
  (setf part (list 'b 'c))
  (setf whole (cons  'a part))
  (tailp part whole)
  (defun our-tailp (x y)
    (format t "~%our-tailp ~a ~a" x y)
    (or (eql x y)
        (and (consp y)
             (our-tailp x (cdr y)))))
  (setf not-part (copy-list part))
  (our-tailp part whole)
  (our-tailp not-part whole)
  (our-tailp nil nil)
  (our-tailp t t)
  (our-tailp (list 'a 'b) (list 'a 'b))
  (setf part (list 'b 'c)
        whole1 (cons 1 part)
        whole2 (cons 2 part))
  (our-tailp part whole1)
  (our-tailp part whole2)
  (our-tailp whole1 whole2)
  (our-tailp whole2 whole1)
  (setf element (list 'a 'b)
        holds1 (list 1 element 2)
        holds2 (list element 3))
  (defun our-copy-list (lst)
    (if (null lst)
      nil
      (cons (car lst) (our-copy-list (cdr lst)))))
  (our-copy-list element)
  (our-copy-list holds1)
  (our-copy-list holds2)
  (defun our-copy-tree (tr)
    (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))
  (our-copy-tree element)
  (our-copy-tree holds1)
  (our-copy-tree holds2)
  (setf whole (list 'a 'b 'c)
        tail (cdr whole))
  (setf (second tail) 'e)
  tail
  whole

)


