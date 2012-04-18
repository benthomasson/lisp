
(load "../util/util.lisp")
(use-package :util)


(print-code
  (defun make-queue () (cons nil nil))

  (defun enqueue (obj q)
    (format t "~%enqueue ~a ~a ~a ~a ~a" obj q (car q) (cdr q) (cdr (cdr q)) )
    (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
    (car q))
  
  (defun dequeue (q)
    (pop (car q)))

  (setf q1 (make-queue))
  (progn (enqueue 'a q1)
         (enqueue 'b q1)
         (enqueue 'c q1))
  q1
  (dequeue q1)
  q1
  (dequeue q1)
  q1
  (dequeue q1)
  q1
  (dequeue q1)
  q1
  (enqueue 'a q1)
  q1
  (enqueue 'b q1)
  q1
  (enqueue 'c q1)
  q1
  (enqueue 'd q1)
  q1
  )
