

(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)



(defun our-copy-tree (tree)
  (if (atom tree)
    tree
    (cons (our-copy-tree (car tree))
          (if (cdr tree) (our-copy-tree (cdr tree))))))

(deftest test-our-copy-tree
         (let* ((old-tree '(a (b c) (((d)))))
               (new-tree (our-copy-tree old-tree)))
           (assert-equal old-tree new-tree)
           (assert-nil (eq old-tree new-tree))
           (assert-true (equal old-tree new-tree))))

(defun count-leaves (tree)
  (if (atom tree)
    1
    (+ (count-leaves (car tree))
       (or (if (cdr tree) (count-leaves (cdr tree)))
           1))))

(deftest test-count-leaves
         (assert-equal (count-leaves '(a b c d e)) 6)
         (assert-equal (count-leaves '(a (b) c d e)) 7)
         (assert-equal (count-leaves '(a (b) c ((d) e))) 9)
         (assert-equal (count-leaves '((a b (c d)) (e) f)) 10))


(defun flatten (tree)
  (if (atom tree)
    (mklist tree)
    (nconc (flatten (car tree))
           (if (cdr tree) (flatten (cdr tree))))))

(deftest test-flatten
         (assert-equal (flatten '((a b (c d)) (e) f ())) '(a b c d e f)))

(defun rfind-if (fn tree)
  (if (atom tree)
    (and (funcall fn tree) tree)
    (or (rfind-if fn (car tree))
        (if (cdr tree) (rfind-if fn (cdr tree))))))


(deftest test-rfind-if
         (assert-equal (rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5)) 3))


(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
                 (if (atom tree)
                   (if (functionp base)
                     (funcall base tree)
                     base)
                   (funcall rec (self (car tree))
                            (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

(setf (symbol-function 'our-copy-tree2) (ttrav #'cons))

(setf (symbol-function 'count-leaves2) (ttrav #'(lambda (l r) (+ l (or r 1))) 1))

(setf (symbol-function 'flatten2) (ttrav #'nconc #'mklist))

(deftest test-our-copy-tree2
         (let* ((old-tree '(a (b c) (((d)))))
               (new-tree (our-copy-tree2 old-tree)))
           (assert-equal old-tree new-tree)
           (assert-nil (eq old-tree new-tree))
           (assert-true (equal old-tree new-tree))))

(deftest test-count-leaves2
         (assert-equal (count-leaves2 '(a b c d e)) 6)
         (assert-equal (count-leaves2 '(a (b) c d e)) 7)
         (assert-equal (count-leaves2 '(a (b) c ((d) e))) 9)
         (assert-equal (count-leaves2 '((a b (c d)) (e) f)) 10))

(deftest test-flatten2
         (assert-equal (flatten2 '((a b (c d)) (e) f ())) '(a b c d e f)))

(defun trec (rec &optional (base #'identity))
  (labels
    ((self (tree)
           (if (atom tree)
             (if (functionp base)
               (funcall base tree)
               base)
             (funcall rec tree
                      #'(lambda ()
                          (self (car tree)))
                      #'(lambda ()
                          (if (cdr tree)
                            (self (cdr tree))))))))
    #'self))

(setf (symbol-function 'flatten3) (trec #'(lambda (o l r) (nconc (funcall l) (funcall r)))
                                        #'mklist))

(defun rfind-if2 (fn lst)
  (funcall (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
                 #'(lambda (tree) (and (oddp tree) tree))) lst))

(deftest test-rfind-if2
         (assert-equal (rfind-if2 (fint #'numberp #'oddp) '(2 (3 4) 5)) 3))

(deftest test-flatten3
         (assert-equal (flatten3 '((a b (c d)) (e) f ())) '(a b c d e f)))


(print-code 
  (compose #'oddp #'truncate)
  #.(compose #'oddp #'truncate))

(print (run-tests))
