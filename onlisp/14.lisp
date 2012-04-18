
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
           (let ((it ,sym)) ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

(print-code
  (aif '(a b c)
       (print it))
  (awhen '(a b c)
         (print it))
  (aif nil
       (print it))
  (awhen nil
       (print it))
  (aand '(a b c)
        (print it)
        (print (reverse it))
        (print it)))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
          ,(funcall (alambda (args)
                             (case (length args)
                               (0 nil)
                               (1 (car args))
                               (t `(let ((it ,(car args)))
                                     ,(self (cdr args))))))
                    args)))


(print-code
  (funcall (alambda (x)
                    (if (= x 0)
                      1
                      (* x (self (1- x))))) 3))


(defun count-instances (obj lists)
  (labels ((instances-in (list)
                         (if list
                           (+ (if (eq (car list) obj) 1 0)
                              (instances-in (cdr list)))
                           0)))
    (mapcar #'instances-in lists)))

(print-code
  (count-instances 'a '((a b c) (d a r p a) (d a r) (a a))))


(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
              (aif2 ,test
                    (progn ,@body)
                    (setq ,flag nil))))))


(defmacro acond2 (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (val (gensym))
          (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
         (if (or ,val ,win)
           (let ((it ,val)) ,@(cdr cl1))
           (acond2 ,@(cdr clauses)))))))

(let ((edible (make-hash-table)))
  (setf (gethash 'olive-oil edible) t
        (gethash 'motor-oil edible) nil)
(print-code
  (defun edible? (x)
    (multiple-value-bind (val found?) (gethash x edible)
      (if found?
        (if val 'yes 'no)
        'maybe)))
  (edible? 'olive-oil)
  (edible? 'motor-oil)
  (edible? 'iguana)
  (mapcar #'edible? '(motor-oil olive-oil iguana))))



(let ((edible (make-hash-table)))
  (setf (gethash 'olive-oil edible) t
        (gethash 'motor-oil edible) nil)
(print-code
  (defun edible? (x)
    (aif2 (gethash x edible)
          (if it 'yes 'no)
          'maybe))
  (edible? 'olive-oil)
  (edible? 'motor-oil)
  (edible? 'iguana)
  (mapcar #'edible? '(motor-oil olive-oil iguana))))


(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
                ,@body))))

(defun our-load (filename)
  (do-file filename (eval it)))

'(print-code 
  (format t "Enter something:")
  (read2))

(our-load "test-file.lisp")

(print (run-tests))



