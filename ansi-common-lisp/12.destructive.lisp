
(load "../util/util.lisp")
(use-package :util)

(print-code 
  (setf lst '(a r a b i a))
  (delete 'a lst)
  lst
  (setf lst (delete 'a lst))
  lst

  (defun nconc2 (x y)
    (if (consp x)
      (progn
        (setf (cdr (last x)) y)
        x)
      y))

  (nconc2 nil nil)
  (nconc2 (list 'a 'b) (list 'c 'd))
  (setf ab (list 'a 'b)
        cd (list 'c 'd))
  (nconc2 ab cd)
  ab
  cd
  (mapcan #'list 
          '(a b c)
          '(1 2 3 4))

  (mapcan #'list 
          '(1 2 3 4)
          '(10 20 30 40))
  (mapcan #'list
          '((a b c) (1 2 3) (x y z)))
  (mapcan #'reverse
          '((a b c) (1 2 3) (x y z)))

  (defun mappend (fn &rest lsts)
    (apply #'append (apply #'mapcar fn lsts)))

  (mappend #'list '(a b c) '(1 2 3) '(x y z))
  (mappend #'reverse '((a b c) (1 2 3) (x y z)))

           

  )
