
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)
(use-package :unit-test)

(defclass basic-block ()
  ((name :accessor block-name :initarg :name)
   (width :accessor block-width :initarg :width)
   (height :accessor block-height :initarg :height)
   (position :accessor block-position :initarg :position)
   (supported-by :accessor block-supported-by :initform nil)))

(defclass movable-block (basic-block) ())

(defclass load-bearing-block (basic-block)
  ((support-for :accessor block-support-for :initform nil)))


(defclass brick (movable-block load-bearing-block) ())

(defclass wedge (movable-block) ())

(defclass ball (movable-block) ())


(defclass table (load-bearing-block) ())

(defclass hand ()
  ((name :accessor hand-name :initarg :name)
   (position :accessor hand-position :initarg :position)
   (grasping :accessor hand-grasping :initform nil)))


(defvar *blocks*
  (list 
    (make-instance 'table :name 'table :width 20 :height 0 :position '(0 0))
    (make-instance 'brick :name 'b1 :width 2 :height 2 :position '(0 0))
    (make-instance 'brick :name 'b2 :width 2 :height 2 :position '(2 0))
    (make-instance 'brick :name 'b3 :width 4 :height 4 :position '(4 0))
    (make-instance 'brick :name 'b4 :width 2 :height 2 :position '(8 0))
    (make-instance 'wedge :name 'w5 :width 2 :height 4 :position '(10 0))
    (make-instance 'brick :name 'b6 :width 4 :height 2 :position '(12 0))
    (make-instance 'wedge :name 'w7 :width 2 :height 2 :position '(16 0))
    (make-instance 'ball :name 'l8 :width 2 :height 2 :position '(18 0))))
    
(dolist (l *blocks*) 
  (set (block-name l) l))


(dolist (l (remove table *blocks*))
  (push l (block-support-for table))
  (setf (block-supported-by l) table))


(defvar *hand* (make-instance 'hand :name '*hand* :position '(0 6)))







