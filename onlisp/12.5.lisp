
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)


(print-code
  (defvar *cache* (make-hash-table))

  (defun retrieve (key)
    (multiple-value-bind (x y) (gethash key *cache*)
      (if y
        (values x y)
        (cdr (assoc key *world*)))))



  (defsetf retrieve (key) (val)
           `(setf (gethash ,key *cache*) ,val))


  (defvar *world* '((a .2) (b . 16) (c . 50) (d . 20) (f . 12))) 
  (retrieve 'c)
  (setf (retrieve 'n) 77)
  (retrieve 'n))

(print (run-tests))
