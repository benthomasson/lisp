
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

;;wrong
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
     ((> ,var limit))
     ,@body))

(print-code
  (macroexpand-1 '(for (x 1 5)
       (princ x)))
  (macroexpand-1 '(for (limit 1 5)
       (princ limit))))


(print (run-tests))
