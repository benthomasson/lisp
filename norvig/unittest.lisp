
(defpackage :unittest
  (:use :cl)
  (:export :print-code
           :test-code
           :report-result))

(in-package :unittest)

(defun print-code-result (body result)
    (format t "~%~a -> ~a" body result ))

(defmacro print-code (&body forms)
`(progn ,@(mapcar (lambda (body) `(print-code-result ',body ,body)) forms )))

(defun report-result (body result)
    (if result 
      (format t "~%passed: ~a" body)
      (format t "~%FAILED: ~a" body)))

(defmacro test-code (&body forms)
`(progn ,@(mapcar (lambda (body) `(report-result ',body ,body)) forms)))


