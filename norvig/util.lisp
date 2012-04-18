
(defpackage :util
  (:use :cl)
  (:export :print-code))

(in-package :util)

(defun print-code-result (body result)
    (format t "~%~a -> ~a" body result ))

(defmacro print-code (&body forms)
`(progn ,@(mapcar (lambda (body) `(print-code-result ',(copy-tree body) ,body)) forms )))


