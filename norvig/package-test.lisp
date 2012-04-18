
(load 'unit-test.lisp)
(use-package :unit-test)

(print *package*)
(print (load 'unittest.lisp))
(print (use-package :unittest))


(deftest test-package
    (assert-equal *package* (find-package :cl-user))
    (assert-not-equal *package* (find-package :unittest)))

(print (run-tests))


