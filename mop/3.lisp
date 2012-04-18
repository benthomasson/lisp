
(load 'unit-test.lisp)
(use-package :unit-test)

(defclass counted-class (standard-class) 
  ((counter :initform 0)))

(defclass rectangle () ())

(defmethod make-instance :after ((class counted-class) &key)
  (incf (slot-value class 'counter)))

;(setf (find-class 'counted-rectangle)
;      (make-instance 'counted-class
;                     :name 'counted-rectangle
;                     :direct-superclasses (list (find-class 'rectangle))
;                     :direct-slots ()))

(defclass counted-rectangle (rectangle)
  ()
  (:metaclass counted-class))

(deftest test-metaobject-class
         (print (class-of (find-class 'rectangle)))
         (print (class-of (find-class 'counted-rectangle)))
         (assert-error (slot-value (find-class 'rectangle) 'counter))
         (print (slot-value (find-class 'counted-rectangle) 'counter))
         (make-instance 'counted-rectangle)
         (print (slot-value (find-class 'counted-rectangle) 'counter)))
         

(print (run-tests))
