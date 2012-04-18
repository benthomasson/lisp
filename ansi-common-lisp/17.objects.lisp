
(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun rget (prop obj)
  (multiple-value-bind (val in) (gethash prop obj)
    (if in
      (values val in)
      (let ((par (gethash :parent obj)))
        (and par (rget prop par))))))

(defun tell (obj message &rest args)
  (apply (rget message obj) obj args))

(deftest test-objects
         (setf circle-class (make-hash-table)
               our-circle (make-hash-table)
               (gethash :parent our-circle) circle-class 
               (gethash 'radius our-circle) 2)
         (assert-equal (rget 'radius our-circle) 2)
         (assert-equal (rget :parent our-circle) circle-class)
         (setf (gethash 'area circle-class) #'(lambda (x)
                                                (* pi (expt (rget 'radius x) 2))))
         (assert-equal (tell our-circle 'area) (* pi (expt 2 2))))


(print (run-tests))
