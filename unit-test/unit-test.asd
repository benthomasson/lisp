
(defpackage #:unit-test-asd
  (:use :cl :asdf))

(in-package :unit-test-asd)

(defsystem unit-test
           :serial t
           :components ((:file "unit-test"))
           :depends-on ("split-sequence"))

(defsystem test-unit-test
           :serial t
           :components ((:file "test-unit-test"))
           :depends-on ("unit-test"))

