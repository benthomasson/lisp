
(load "../util/util.lisp")
(use-package :util)
(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(deftest test-setf
         (assert-equal (setq lst '(a b c)) '(a b c))
         (assert-equal lst '(a b c))
         (assert-equal (setf (car lst) 480) 480)
         (assert-equal lst '(480 b c))
         (assert-equal (rplaca lst 1) '(1 b c))
         (assert-equal lst '(1 b c)))

(defmacro toggle (obj)
  `(setf ,obj (not ,obj)))

(deftest test-toggle
         (let ((lst '(a b c)))
           (toggle (car lst))
           (assert-equal lst '(nil b c))))

(defmacro friend-of (p q)
  `(gethash ,p (gethash ,q *friends*)))

(deftest test-toggle2
         (defvar *friends* (make-hash-table))
         (setf (gethash 'mary *friends*) (make-hash-table))
         (setf (gethash 'john (gethash 'mary *friends*)) t)
         (assert-true (friend-of 'john 'mary))
         (toggle (friend-of 'john 'mary))
         (assert-nil (friend-of 'john 'mary)))

(print-code
  (macroexpand-1 '(toggle (nth (incf i) lst))))

(deftest test-toggle3
         (let ((lst '(t nil t))
               (i -1))
           (toggle (nth (incf i) lst))
           (assert-equal lst '(t nil t))))

(define-modify-macro toggle2 () not)

(print-code
  (macroexpand-1 '(toggle2 (nth (incf i) lst))))

(deftest test-toggle4
         (let ((lst '(t nil t))
               (i -1))
           (toggle2 (nth (incf i) lst))
           (assert-equal lst '(nil nil t))))
         

(print (run-tests))
