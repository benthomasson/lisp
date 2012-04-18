
(load 'unittest.lisp)

(use-package :unittest)

(print-code (setf a 5))
(test-code (= a 5))

(print-code (setf b 6))
(test-code (= a 5)
           (= b 6))

(print-code (setf c 7)
            (incf c))

(test-code (= c 8))

(print-code (setf d 8)
            (incf d)
            (decf d))

(test-code (= d 8))

(test-code (= 1 1))
(test-code (= 1 2))
(test-code (> 1 2))
(test-code (< 1 2))
(test-code (< 2 2))
(test-code (< 1 1)
           (< 1 0)
           (> 1 0)
           (> 0 1)
           (< 0 1))

(defmacro make-test (body)
    `(setf a-test (lambda () ,body)))

(print-code (macroexpand-1 '(make-test (= 1 1))))

(make-test (= 1 1))

(print (funcall a-test))

(setf something nil)

(test-code (null something))

(setf something (list* 'a something ))
(print something)

(test-code (not (null something)))
(test-code (equalp something '(a) ))

(setf something nil)
(test-code (null something))
(test-code (boundp something))

(push 'a something)

(print something)
(test-code (not (null something)))
(test-code (equalp something '(a) ))

(assert t (t) "hi" )
(handler-case
    (assert nil () "hello")
    ;;(setf a 5)
    (error (err) 
        (print err))
    (:no-error (return) return))


(defun assert-true ( bool message )
  (assert bool () message ))

(defun assert-false ( bool message )
  (assert (not bool) () message ))

(assert-true t "pass")
(handler-case 
(assert-true nil "fail")
    (error (err)))

(test-code (typep 1 'integer)
           (typep t 'symbol))

;;(test-code (assert-true nil "fail"))

(setf body '(assert-true nil "fail"))

(print `(handler-case
          ,body
          (error (err))))

(defmacro run-a-test (body)
  `(handler-case 
     ,body
     (error (err) 
        (format t "~%FAILED: ~a -> ~a" ',body err))
     (:no-error (return)
        (format t "~%passed: ~a -> ~a" ',body return))))

(defmacro run-tests (&body forms)
    `(progn ,@(mapcar (lambda (body) `(run-a-test ,body)) forms)))

(run-a-test (assert-true nil "fail"))
(run-a-test (assert-true t "fail"))
(run-a-test (error "error"))

(run-tests (assert-true nil "fail")
           (assert-false t "fail")
           (assert-true (= 1 1) "is 1 really 1?"))

;;(print (funcall (lambda (body) `(run-a-test ,body)) '(assert-true nil "fail")))

(defmacro dev-macro (lambda &rest args)
  `(print (funcall ,lambda ,@args)))

(dev-macro (lambda (body) `(run-a-test ,body)) '(assert-true nil "fail"))

(defclass test () ())

(defmethod run ((a-test test))
  (print a-test))

(setf some-test (make-instance 'test))

(print-code (run some-test))

(setf some-lambda-test (lambda () 
           (assert-true nil "fail lambda")))

(handler-case
    (funcall some-lambda-test)
    (error (err)
        (format t "~%FAILED: ~a" err)))

(defmacro run-lambda-test (name body) 
  `(handler-case 
     (funcall ,body)
     (error (err) 
        (format t "~%FAILED: ~a -> ~a" ',name err))
     (:no-error (return)
        (format t "~%passed: ~a -> ~a" ',name return))))
  
(run-lambda-test some-lambda-test some-lambda-test)

(run-lambda-test eh (lambda () 
           (assert-true nil "fail lambda")))

(defun some-fun-test () 
  (assert-true nil "fail some fun test"))

(run-lambda-test some-fun-test #'some-fun-test)

(defmacro make-test (body)
  `(lambda () ,body))

(setf *tests* nil)

;;(print-code (macroexpand-1 '(make-test (assert-true t "pass"))))
;;(setf *tests* (list* (make-test (assert-true t "pass")) *tests* ))

(defmacro add-test (&body forms)
`(progn ,@(mapcar (lambda (body) `(setf *tests* (list* (list ',body (make-test ,body)) *tests* ))) forms)))

(add-test (assert-true nil "fail add-test"))
(add-test (assert-false nil "pass add-test"))

(add-test (assert-true nil "multiple1")
          (assert-false nil "multiple2"))

(defmacro add-test-1 (&body forms)
`(progn ,@(mapcar (lambda (body) `(setf *tests* (list* (list ',body (make-test ,body)) *tests* ))) forms)))

(print *tests*)

;;(run-lambda-test *tests* *tests*)

(defun run-lambda-test-1 (name body) 
  (handler-case 
     (funcall body)
     (error (err) 
        (format t "~%FAILED: ~a -> ~a" name err))
     (:no-error (return)
        (format t "~%passed: ~a -> ~a" name return))))

(defun run-all-tests () 
(dolist (test *tests*) 
  (run-lambda-test-1 (first test) (second test))))

(run-all-tests)



