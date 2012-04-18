
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defmacro fn (expr) 
  `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
    expr
    (if (eq (car expr) 'compose)
      (build-compose (cdr expr))
      (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                      (if fns
                        `(,(rbuild (car fns))
                           ,(rec (cdr fns)))
                        g)))
          (rec fns)))))


(mac-debug (fn (and integerp oddp)))
(mac-debug (fn (compose list 1+ truncate)))
(mac-debug (fn (or (and integerp oddp)
                     (and consp cdr))))
         
(print-code
  (mapcar (fn (and integerp oddp 1+)) '(1.4 2 3 yak))
  (mapcar (fn (compose (lambda (x) (+ x 3)) truncate)) '(1.4 2.1 3.9))
  (mapcar (fn (if oddp 1+ 1-)) '(1 2 3 4 5 6))
  (mapcar (fn (list 1- identity 1+)) '(10 20 30))
  (mapcar (fn (or integerp symbolp)) '(c 3 p 0.2))
  (remove-if (fn (or (and integerp oddp)
                     (and consp cdr)))
                 '(1 (a b) c (d) 2 3.4 (e f g)))
  (mapcar (fn (compose list 1+ truncate)) '(1.4 2.1 3.9 4.5)))


(defmacro alrec (rec &optional base)
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                                ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

(defun our-length (lst)
  (on-cdrs (1+ rec) 0 lst))

(defun our-every (fn lst)
  (on-cdrs (and (funcall fn it) rec) t lst))

(defun our-copy-list (lst)
  (on-cdrs (cons it rec) nil lst))

(defun our-remove-duplicates (lst)
  (on-cdrs (adjoin it rec) nil lst))

(defun our-find-if (fn lst)
  (on-cdrs (if (funcall fn it) it rec) nil lst))

(defun our-some (fn lst)
  (on-cdrs (or (funcall fn it) rec) nil lst))

(print-code
  (our-length '(a b c d e))
  (our-every #'oddp '(1 3 5 7 9))
  (our-every #'oddp '(1 3 5 7 9 10))
  (our-copy-list '(a b c d e f))
  (our-remove-duplicates '(a b a b a c d e f))
  (our-find-if #'oddp '(2 4 6 3 5 7))
  (our-some #'oddp '(2 4 6 8 3 10))
  (our-some #'oddp '(2 4 6 8 10)))

(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx min) rec
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))

(defmacro atrec (rec &optional (base 'it))
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                                ,rec))
           #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

(defun our-copy-tree (tree)
  (on-trees (cons left right) it tree))

(defun count-leaves (tree)
  (on-trees (+ left (or right 1)) 1 tree))

(defun flatten (tree)
  (on-trees (nconc left right) (mklist it) tree))

(defun rfind-if (fn tree)
  (on-trees (or left right)
            (and (funcall fn it) it)
            tree))

(print-code 
  (our-copy-tree '(a b (c) (((D))) ((E F (G)))))
  (count-leaves '(a b (c) (((D))) ((E F (G)))))
  (flatten '(a b (c) (((D))) ((E F (G)))))
  (rfind-if #'(lambda (x) (eq x 'f)) '(a b (c) (((D))) ((E F (G))))))

(defconstant unforced (gensym))

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             #'(lambda ()
                 (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
    (if (eq (delay-forced x) unforced)
      (funcall (delay-closure x))
      (delay-forced x))
    x))

(print-code 
  (let ((x 2))
    (setq d (delay
              (progn
                (print 'called)
                (1+ x)))))
  (force 'a)
  (force d)
  (force d)
  d)




(print (run-tests))



