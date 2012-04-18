
(defpackage #:prolog-asd
  (:use :cl :asdf))

(in-package :prolog-asd)

(defsystem prolog
           :serial t
           :components ((:file "prolog.1" :in-order-to ((compile-op (load-source-op "prolog.1")))))
           :depends-on ("util"))



