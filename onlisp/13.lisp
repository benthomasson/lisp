
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun avg1 (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro avg2 (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defun most-of1 (&rest args)
  (let ((all 0)
        (hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

(defmacro most-of2 (&rest args)
  (let ((need (floor (/ (length args) 2)))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
                         `(and ,a (> (incf ,hits) ,need)))
                     args)))))

(deftest test-avg-most-of
  (assert-equal (avg1 1 2 3 4) 5/2)
  (assert-equal (avg2 1 2 3 4) 5/2)
  (assert-true (most-of1 t t nil))
  (assert-nil (most-of1 t nil nil))
  (assert-true (most-of2 t t nil))
  (assert-nil (most-of2 t nil nil)))

(print-code
  (avg1 1 2 3 4)
  (avg2 1 2 3 4)
  (most-of1 t t nil)
  (most-of1 t nil nil)
  (most-of2 t t nil)
  (most-of2 t nil nil))


(defun nthmost1 (n lst)
  (nth n (sort (copy-list lst) #'>)))


(print (run-tests))
