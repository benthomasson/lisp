
(load "/Users/ben/.clisprc.lisp")

(use-package :util)
(asdf-load :csc505-util)
(use-package :csc505-util)
(asdf-load :unit-test)
(use-package :unit-test)


(print (run-tests))
