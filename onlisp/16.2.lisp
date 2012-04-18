
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)


(defmacro defanaph (name &optional &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun anaphex1 (args call)
  (if args
    (let ((sym (gensym)))
      `(let* ((,sym ,(car args))
              (it ,sym))
         ,(anaphex1 (cdr args)
                    (append call (list sym)))))
    call))

(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defanaph alist)
(defanaph aif :rule :first)
(defanaph asetf :rule :place)

(defmacro our-incf (place &optional (val 1))
  `(asetf ,place (+ it ,val)))

(defmacro pull (obj place &rest args)
  `(asetf ,place (delete ,obj it ,@args)))

(print-code
  (setf x 1)
  (our-incf x)
  x
  (our-incf x 5 )
  x
  (our-incf x)
  (our-incf x)
  (our-incf x)
  (our-incf x))



(print (run-tests))



