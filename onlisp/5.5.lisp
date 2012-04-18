
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defun lrec (rec &optional base)
  (labels ((self (lst)
                 (if (null lst)
                   (if (functionp base)
                     (funcall base)
                     base)
                   (funcall rec (car lst)
                            #'(lambda ()
                                (self (cdr lst)))))))
    #'self))

(setf (symbol-function 'our-length) (lrec #'(lambda (x f) (1+ (funcall f))) 0))

(deftest test-our-length
         (assert-equal (our-length '(1 2 3 4 5)) 5))

(defun our-every (predicate lst)
  (funcall (lrec #'(lambda (x f) (and (funcall predicate x) (funcall f))) t) lst))

(deftest test-our-every
         (assert-equal (our-every #'oddp '(1 3 5 7 9)) t))


(setf (symbol-function 'our-copy-list) 
      (lrec #'(lambda (x f) (cons x (funcall f)))))

(deftest test-our-copy-list
         (let* ((old-list '(a b c d e))
                (new-list (our-copy-list old-list)))
           (assert-equal new-list old-list)
           (assert-nil (eq new-list old-list))
           (assert-true (equal new-list old-list))))

(setf (symbol-function 'our-remove-duplicates)
      (lrec #'(lambda (x f) (adjoin x (funcall f)))))

(deftest test-our-remove-duplicates
         (assert-equal (our-remove-duplicates '(a b c)) '(a b c))
         (assert-equal (our-remove-duplicates '(a a a)) '(a))
         (assert-equal (our-remove-duplicates '(a b b c c)) '(a b c)))


(defun our-find-if (fn lst)
  (funcall (lrec #'(lambda (x f) (if (funcall fn x) x (funcall f)))) lst ))

(deftest test-our-find-if
         (assert-equal (our-find-if #'oddp '(2 4 6 5 7 9 11)) 5)
         (assert-equal (our-find-if #'zerop '(2 4 6 5 7 9 11)) nil))

(defun our-some (fn lst)
  (funcall (lrec #'(lambda (x f) (or (funcall fn x) (funcall f)))) lst ))
  
(deftest test-our-some
         (assert-true (our-some #'oddp '(2 4 6 5 7 9 11)))
         (assert-nil (our-some #'zerop '(2 4 6 5 7 9 11))))

(print (run-tests))
