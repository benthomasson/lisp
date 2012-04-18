
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)

(use-package :unit-test)

(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (pred &rest args)
  `(progn
     (db-push ',pred ',args)
     ',args))

(print-code
  (fact painter reynolds joshua english)
  (fact painter canale antonio venetian)
  (db-query 'painter)
  (db-query 'cat))



(print (run-tests))
