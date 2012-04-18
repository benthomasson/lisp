
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun compose (&rest fns)
  (if fns
    (let ((fn1 (car (last fns)))
          (fns (butlast fns)))
      #'(lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
    #'identity))


(deftest test-reduce
         (assert-equal (reduce #'+ '(1 2 3 4)) 10)
         (assert-equal (reduce #'+ '(1 2 3 4) :from-end t) 10)
         (assert-equal (reduce #'+ '(1 2 3 4) :initial-value 0) 10)
         (assert-equal (reduce #'- '(1 2 3 4)) -8)
         (assert-equal (reduce #'- '(1 2 3 4) :from-end t) -2)
         (assert-equal (reduce #'- '(1 2 3 4) :initial-value 0) -10))

(deftest test-compose
         (assert-equal (find-if #'oddp '(2 3 4)) 3)
         (assert-equal (1+ (find-if #'oddp '(2 3 4))) 4)
         (assert-equal (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4)) 4)
         (assert-equal (funcall (compose #'1+ #'1+ #'find-if) #'oddp '(2 3 4)) 5)
         (assert-equal (funcall (compose #'1- #'1+ #'1+ #'find-if) #'oddp '(2 3 4)) 4)
         (assert-equal (funcall (compose #'/ #'1+ #'1+ #'find-if) #'oddp '(2 3 4)) 1/5))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
    fn
    (let ((chain (apply #'fun fns)))
      #'(lambda (x)
          (or (funcall fn x) (funcall chain x))))))


(deftest test-fif
         (assert-equal (mapcar (fif #'oddp #'1+ #'identity) '(1 2 3 4 5 6)) '(2 2 4 4 6 6)))

(deftest test-fint
         (assert-equal (find-if (fint #'oddp #'(lambda (x) (= x 5))) '(1 2 3 4 5 6 7)) 5))

(deftest test-fun
         (assert-equal (find-if (fun #'oddp #'(lambda (x) (= x 5))) '(1 2 3 4 5 6 7)) 1))


(print (run-tests))
