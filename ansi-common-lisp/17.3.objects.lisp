
(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defvar *objs* nil)

(defun parents (obj) (gethash :parents obj))

(defun precedence (obj)
  (labels ((traverse (x)
                     (cons x
                           (mapcan #'traverse 
                                   (gethash :parents x)))))
    (delete-duplicates (traverse obj))))

(defun (setf parents) (val obj)
  (prog1 (setf (gethash :parents obj) val)
    (make-precedence obj)))

(defun make-precedence (obj)
  (setf (gethash :preclist obj) (precedence obj))
  (dolist (x *objs*)
    (if (member obj (gethash :preclist x))
      (setf (gethash :preclist x) (precedence x)))))

(defun obj (&rest parents)
  (let ((obj (make-hash-table)))
    (push obj *objs*)
    (setf (parents obj) parents)
    obj))

(defun rget (prop obj)
  (dolist (c (gethash :preclist obj))
    (multiple-value-bind (val in) (gethash prop c)
      (if in (return (values val in))))))

(defmacro defprop (name &optional meth?)
  `(progn
     (defun ,name (obj &rest args)
       ,(if meth?
          `(run-methods obj ',name args)
          `(rget ',name obj)))
     (defun (setf ,name) (val obj)
       (setf (gethash ',name obj) val))))

(defun run-methods (obj name args)
  (let ((meth (rget name obj)))
    (if meth
      (apply meth obj args)
      (error "No ~A method for ~A." name obj))))

(defmacro defmeth (name obj parms &rest body)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (setf (gethash ',name ,gobj)
             (labels ((next () (get-next ,gobj ',name)))
                #'(lambda ,parms ,@body))))))

(defun get-next (obj name)
  (some #'(lambda (x) (gethash name x))
        (cdr (gethash :preclist obj))))

(deftest test-objects
           (setf scoundrel (obj)
                 patriot (obj)
                 patriotic-scoundrel (obj scoundrel patriot))
           (defprop serves)
           (setf (serves scoundrel) 'self
                 (serves patriot) 'country)
           (assert-equal (serves patriotic-scoundrel) 'self)
           (assert-equal (serves patriot) 'country)
           (assert-equal (serves scoundrel) 'self))
    
(deftest test-methods
         (setf circle-class (obj)
               our-circle (obj circle-class)
               grumpy-circle (obj circle-class))
         (defprop radius)
         (defprop area t)
         (setf (radius our-circle) 2)
         (setf (radius grumpy-circle) 3)
         (defmeth area circle-class (c)
                  (* pi (expt (radius c) 2)))
         (defmeth area grumpy-circle (c)
                  (format t "How dare you sterotype me!~%")
                  (funcall (next) c))
         (assert-equal (radius our-circle) 2)
         (assert-equal (radius grumpy-circle) 3)
         (assert-equal (area our-circle) (* pi (expt 2 2)))
         (assert-equal (area grumpy-circle) (* pi (expt 3 2))))

(print (run-tests))
