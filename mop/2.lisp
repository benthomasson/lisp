
(load 'util)
(use-package :util)

(load 'unit-test.lisp)
(use-package :unit-test)

(defun subclasses* (class)
  (remove-duplicates
    (cons class
          (mappend #'subclasses* (class-direct-subclasses class)))))

(defun subclasses (class)
  (remove class (subclasses* class)))

(defun generate-defclass (class)
  `(defclass ,(class-name class)
     ,(mapcar #'class-name (class-direct-superclasses class))
     ,(mapcar #'generate-slot-specification (class-direct-slots class))))

(defun generate-slot-specification (slot)
  `(,(slot-definition-name slot)
     ,@(awhen (slot-definition-initfunction slot)
              `(:inifform ,it))
     ,@(awhen (slot-definition-initargs slot)
              (mappend #'(lambda (initarg) `(:initarg ,initarg)) it))
     ,@(awhen (slot-definition-readers slot)
              (mappend #'(lambda (reader) `(:reader ,reader)) it))
     ,@(awhen (slot-definition-writers slot)
              (mappend #'(lambda (writer) `(:writer ,writer)) it))))

(defun in-order-p (c1 c2)
  (flet ((in-order-at-subclasses-p (sub)
                                   (let ((cpl (class-precedence-list sub)))
                                     (not (null (member c2 (cdr (member c1 cpl))))))))
    (or (eq c1 c2)
        (every #'in-order-at-subclasses-p
               (intersection (subclasses* c1)
                             (subclasses* c2))))))


(defun generate-defgeneric (gf)
  `(defgeneric ,(generic-function-name gf)
               ,(generic-function-lambda-list gf)))

(defun generate-defmethod (method &key show-body)
  `(defmethod ,(generic-function-name (method-generic-function method))
     ,@(method-qualifiers method)
     ,(generate-specialized-arglist method)
     ,@(when show-body (list (method-body method)))))

(defun generate-specialized-arglist (method)
  (let* ((specializers (method-specializers method))
         (lambda-list (method-lambda-list method))
         (number-required (length specializers)))
    (append (mapcar #'(lambda (arg class)
                        (if (eq class (find-class 't))
                          arg
                          `(,arg ,(class-name class))))
                    (subseq lambda-list 0 number-required)
                    specializers)
            (subseq lambda-list number-required))))

(defun display-generic-function (gf-name &key show-body)
  (display-defgeneric gf-name)
  (dolist (method (generic-function-methods (fdefinition gf-name)))
    (print (generate-defmethod method :show-body show-body)))
  (values))


(defun display-defgeneric (gf-name)
  (print (generate-defgeneric (fdefinition gf-name)))
  (values))

(deftest test-find-class
        (print (class-of 'A))
        (assert-error (find-class 'X))
        (assert-nil (find-class 'X nil))
        (assert-equal (find-class 'symbol) (class-of 'A))
        (setf class-a (defclass A () ()))
        (setf a (make-instance 'A))
        (assert-equal (find-class 'A) class-a 1)
        (assert-equal (class-of a) (find-class 'A) 2)
        (assert-equal (class-of a) class-a 3))

(deftest test-class
         (defclass A () ())
         (assert-equal 'A (class-name (find-class 'A)))
         (assert-equal (list (find-class 'standard-object))
                       (class-direct-superclasses (find-class 'A)))

         (assert-nil (class-direct-slots (find-class 'A)))
         (assert-equal (mapcar #'find-class '(A standard-object t))  
                       (class-precedence-list (find-class 'A))))


(deftest test-subclasses*
         (defclass A () ())
         (defclass B (A) ())
         (assert-equal (list (find-class 'B)) (subclasses* (find-class 'B)))
         (assert-equal (list (find-class 'A) (find-class 'B)) (subclasses* (find-class 'A)))
         (assert-equal nil (subclasses (find-class 'B)))
         (assert-equal (list (find-class 'B)) (subclasses (find-class 'A))))

(deftest test-generate-defclass 
         (defclass A () ())
         (defclass B (A) ((x :accessor x)(y :accessor y)))
         (defclass C (B A) ())
         (print (generate-defclass (find-class 'A)))
         (print (generate-defclass (find-class 'B)))
         (print (generate-defclass (find-class 'C)))
         (assert-true (in-order-p (find-class 'B) (find-class 'A)))
         (assert-nil (in-order-p (find-class 'A) (find-class 'B)))
         (assert-true (in-order-p (find-class 'C) (find-class 'A)))
         (assert-true (in-order-p (find-class 'C) (find-class 'B))))


(print (run-tests))
