
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defmacro allf (val &rest args)
  (util:with-gensyms (gval)
                `(let ((,gval ,val))
                   (setf ,@(mapcan #'(lambda (a) (list a gval))
                                   args)))))

(defmacro nilf (&rest args) 
  `(allf nil ,@args))

(defmacro tf (&rest args)
  `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) 
                   `(toggle2 ,a))
               args)))

(define-modify-macro toggle2 () not)

(deftest test-nilf
         (nilf x y z)
         (assert-nil x)
         (assert-nil y)
         (assert-nil z))


(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
                     (lambda (place obj)
                       (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
                     (lambda (place obj &rest args)
                       (unless (apply #'member obj place args)
                         (nconc place (list obj)))))


(print (run-tests))
