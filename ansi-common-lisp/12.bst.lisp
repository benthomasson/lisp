
(load "../util/util.lisp")
(use-package :util)

(print-code
  (defun bst-insert! (obj bst <)
    (if (null bst)
      (make-node :elt obj)
      (progn (bsti obj bst <)
             bst)))
  )
