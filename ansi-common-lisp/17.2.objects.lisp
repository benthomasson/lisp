
(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun tell (obj message &rest args)
  (apply (rget message obj) obj args))

(defun rget (prop obj)
  (dolist (c (precedence obj))
  (multiple-value-bind (val in) (gethash prop c)
    (if in (return (values val in))))))

(defun precedence (obj)
  (labels ((traverse (x)
                     (cons x
                           (mapcan #'traverse 
                                   (gethash :parents x)))))
    (delete-duplicates (traverse obj))))

(deftest test-objects
         (setf scoundrel (make-hash-table)
               patriot   (make-hash-table)
               patriotic-soundrel (make-hash-table)
               (gethash 'serves scoundrel) 'self
               (gethash 'serves patriot) 'country
               (gethash :parents patriotic-soundrel) (list scoundrel patriot))
         (assert-equal (rget 'serves patriotic-soundrel) 'self )
         (assert-equal (rget 'serves patriot) 'country )
         (assert-equal (rget 'serves scoundrel) 'self ))
         

(print (run-tests))
