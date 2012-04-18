
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun find2 (fn lst)
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
        (values (car lst) val)
        (find2 fn (cdr lst))))))


(deftest test-find2
         (let ((*towns* (list 'raleigh 'paris))
               (*bookstores* (make-hash-table)))
               (setf (gethash 'raleigh *bookstores*) 'el-books)
               (setf (gethash 'paris *bookstores*) 'le-books)

               (labels ((bookstores (town)
                 (gethash town *bookstores*)))

               (assert-nil (bookstores 'cary))
               (assert-equal (bookstores 'raleigh) 'el-books)
               (assert-equal (bookstores 'paris) 'le-books)
               (multiple-value-bind (yes value) (find2 #'bookstores '(cary paris))
                 (assert-equal yes 'paris)
                 (assert-equal value 'le-books))
               (multiple-value-bind (yes value) (find2 #'bookstores '(not-a-town raleigh))
                 (assert-equal yes 'raleigh)
                 (assert-equal value 'el-books)))))

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(deftest test-last1
         (assert-equal (last1 '(a b c d e f)) 'f)
         (assert-equal (last1 nil) nil))

(deftest test-single
         (assert-nil (single nil))
         (assert-nil (single 'a))
         (assert-nil (single '(a b c)))
         (assert-true (single '(a)))
         (assert-equal (single '(b)) 't))

(deftest test-append1
         (assert-equal (append1 '(1 2 3) 4) '(1 2 3 4))
         (assert-equal (append1 nil 'a) '(a))
         (assert-equal (append1 nil nil) '(nil)))

(deftest test-conc1
         (assert-equal (conc1 '(1 2 3) 4) '(1 2 3 4))
         (assert-equal (conc1 nil 'a) '(a))
         (let ((lst '(a b c)))
           (assert-equal (conc1 lst 'd) '(a b c d))
           (assert-equal lst '(a b c d))))

(deftest test-mklist 
         (assert-equal (mklist nil) nil)
         (assert-equal (mklist 1) '(1))
         (assert-equal (mklist '(1)) '(1))
         (assert-equal (mklist '(a b c)) '(a b c)))

(defun longer (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
      (compare x y)
      (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                    (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(deftest test-longer
         (assert-true (longer '(1 2 3) '(1 2)))
         (assert-nil (longer '(1 2 3) '(1 2 3)))
         (assert-nil (longer '(1 2) '(1 2 3)))
         (assert-true (longer '(1) nil))
         (assert-nil (longer nil nil))
         (assert-error (longer '(1) 1))
         (assert-error (longer 1 nil)))

(deftest test-filter
         (assert-equal (filter #'(lambda (x) (if (numberp x) (1+ x))) '(a 1 2 b 3 c d 4)) '(2 3 4 5)))

(deftest test-group 
         (assert-equal (group '(a b c d e f g) 2) '((a b) (c d) (e f) (g)))
         (assert-equal (group '(a b c d e f g) 1) '((a) (b) (c) (d) (e) (f) (g)))
         (assert-equal (group '(a b c d e f g) 3) '((a b c) (d e f) (g)))
         (assert-equal (group '(a b c d e f g) 4) '((a b c d) (e f g)))
         (assert-error (group '(a b c d e f g) 0)))


(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
                (cond ((null tree) (nreverse acc))
                     ((consp (car tree))
                      (rec (cdr tree)
                           (cons (rec (car tree) nil) acc)))
                     (t (rec (cdr tree)
                             (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))


(deftest test-flatten
         (assert-equal (flatten '(a (b c) ((d e) f))) '(a b c d e f)))

(deftest test-prune
         (assert-equal (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))) '(1 (3 (5)) 7 (9))))

(defun find2 (fn lst)
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
        (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
      ((or (null src) (funcall fn (car src)))
       (values (nreverse acc) src))
      (push (car src) acc))))


(deftest test-before
         (assert-true (before 'b 'd '(a b c d)))
         (assert-equal (before 'b 'd '(a b c d)) '(b c d))
         (assert-nil (before 'd 'b '(a b c d)))
         (assert-true (before 'a 'b '(a))))

(deftest test-after
         (assert-nil (after 'b 'd '(a b c d)))
         (assert-true (after 'd 'b '(a b c d)))
         (assert-equal (after 'd 'b '(a b c d)) '(d))
         (assert-nil (after 'a 'b '(a))))

(deftest test-duplicate
         (assert-true (duplicate 'a '(a b c a d)))
         (assert-equal (duplicate 'a '(a b c a d)) '(a d)))

(deftest test-split-if
         (multiple-value-bind (first second) (split-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7 8 9 10))
           (assert-equal first '(1 2 3 4))
           (assert-equal second '(5 6 7 8 9 10))))

(defun most (fn lst)
  (if (null lst)
    (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max score))))
      (values wins max))))

(defun best (fn lst)
  (if (null lst)
    nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
          (setq wins obj)))
      wins)))

(defun mostn (fn lst)
  ;(format t "~%mostn: ~a ~a" fn lst)
  (if (null lst)
    (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      ;(format t "~%result: ~a max: ~a" result max)
      (dolist (obj (cdr lst))
        ;(format t "~%obj:~a" obj)
        (let ((score (funcall fn obj)))
          ;(format t "~%score: ~a" score)
          (cond ((> score max)
                 (setq max score
                       result (list obj)))
                ((= score max)
                 (push obj result)))))
      (values (nreverse result) max))))


(deftest test-most
         (multiple-value-bind (lst length) (most #'length '((a b) (a b c) (a) (e f g)))
           (assert-equal lst '(a b c))
           (assert-equal length 3)))

(deftest test-best
         (assert-equal (best #'> '(1 2 3 4 5)) 5))

(deftest test-mostn
         (multiple-value-bind (lst length) (mostn #'length nil)
           (assert-equal lst nil)
           (assert-equal length nil))
         (multiple-value-bind (lst length) (mostn #'length '((x)))
           (assert-equal lst '((x)))
           (assert-equal length 1))
         (multiple-value-bind (lst length) (mostn #'length '((a b) (a b c) (a) (e f g)))
           (assert-equal lst '((a b c) (e f g)))
           (assert-equal length 3)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  ;(format t "mapa-b ~a ~a ~a ~a" fn a b step)
  (do ((i a (+ i step))
       (result nil))
    ((> i b) (nreverse result))
    ;(format t "~%i:~a result:~a" i result)
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
    ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
    (apply fn args)
    (apply #'mapcar #'(lambda (&rest args)
                        (apply #'rmapcar fn args))
           args)))


(deftest test-map0-n
         (assert-equal (map0-n #'1+ 5) '(1 2 3 4 5 6)))
    
(deftest test-map1-n
         (assert-equal (map1-n #'1+ 5) '(2 3 4 5 6)))

(deftest test-mapa-b
         (assert-equal (mapa-b #'1+ -2 0 .5) '(-1 -0.5 0.0 0.5 1.0)))

(defun mapa-b2 (fn a b &optional (step 1))
  (map-> fn
         a
         #'(lambda (x) (> x b))
         #'(lambda (x) (+ x step))))

(deftest test-map->
         (assert-equal (mapa-b2 #'1+ -2 0 .5) '(-1 -0.5 0.0 0.5 1.0)))


(deftest test-rmapcar
         (assert-equal (rmapcar #'1+ '(1 2 (3 4 (5) 6) 7 (8 9))) '(2 3 (4 5 (6) 7) 8 (9 10)))
         (assert-equal (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40))) '(11 (22 (33) 44))))
         

(defun readlist (&rest args)
  (values (read-from-string
            (concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
        (return)
        (format *query-io* "~A~%" (funcall fn in))))))

'(deftest test-readlist
         (format t "~%Please type: test~%")
         (assert-equal (readlist) '(test)))

'(deftest test-prompt 
         (assert-equal (prompt "~%Please type: (test)~%") '(test)))

'(deftest test-break-loop
         (break-loop #'print #'(lambda (x) (eq x :q)) "~%Type :q to break:"))



(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                      (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

(deftest test-mkstr 
         (assert-equal (mkstr pi "pieces of " 'pi) "3.1415926535897932385L0pieces of PI"))

(deftest test-symb
         (assert-equal (symb 'ar "Madi" #\L #\L 0) '|ARMadiLL0|))

(deftest test-explode
         (assert-equal (explode 'bomb) '(b o m b)))


(print (run-tests))


