
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

(abbrevs dbind destructuring-bind
         mvbind multiple-value-bind
         mvsetq multiple-value-setq)

(print-code
  (dbind (x (y . z)) '(1 (2 3 4 5))
         (values x y z))
  (mvbind (x y z) (values 1 2 3)
          (values x y z))
  (mvsetq (x y z) (values 1 2 3))
  (values x y z))

(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
               props)))
         
(print-code
  (propmacros color shape)
  (setf (color 'ball1) 'red)
  (color 'ball1)
  (mac-debug (color 'ball1))
  (setf (shape 'ball1) 'sphere)
  (shape 'ball1))


(defmacro a+ (&rest args)
  (a+expand args nil))


(defun a+expand (args syms)
  (if args
    (let ((sym (gensym)))
      `(let* ((,sym ,(car args))
              (it ,sym))
         ,(a+expand (cdr args)
                    (append syms (list sym)))))
    `(+ ,@syms)))

(defmacro alist (&rest args)
  (alist-expand args nil))

(defun alist-expand (args syms)
  (if args
    (let ((sym (gensym)))
      `(let* ((,sym ,(car args))
              (it ,sym))
         ,(alist-expand (cdr args)
                        (append syms (list sym)))))
    `(list ,@syms)))


(defun mass-cost (menu-price)
  (a+ menu-price (* it .05) (* it 3)))

(print-code
  (mass-cost 7.95)
  (macroexpand '(a+ menu-price (* it 0.05) (* it 3)))
  (alist 1 (+ 2 it) (+ 2 it))
  (macroexpand '(alist 1 (+ 2 it) (+ 2 it))))


(defmacro defanaph (name &optional calls)
  (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))

(defun anaphex (args expr)
  (if args 
    (let ((sym (gensym)))
      `(let* ((,sym ,(car args))
              (it ,sym))
         ,(anaphex (cdr args)
                   (append expr (list sym)))))
    expr))

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defanaph a+)
(defanaph alist)

(defun mass-cost (menu-price)
  (a+ menu-price (* it .05) (* it 3)))

(print-code
  (mass-cost 7.95)
  (macroexpand '(a+ menu-price (* it 0.05) (* it 3)))
  (alist 1 (+ 2 it) (+ 2 it))
  (macroexpand '(alist 1 (+ 2 it) (+ 2 it))))
                    


(print (run-tests))



