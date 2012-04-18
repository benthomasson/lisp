
(defpackage #:util-asd
  (:use :cl :asdf))

(in-package :util-asd)

(defsystem util
           :serial t
           :components ((:file "util")
				  (:file "simple-stream")))

(defsystem test-util
           :serial t
           :components ((:file "test-util"))
           :depends-on ("util" "unit-test"))

