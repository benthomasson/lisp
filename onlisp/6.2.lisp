
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (if yes
#'(lambda ()
    (format t "~A~%>>" conts)
    (case (read)
      (yes (funcall (gethash yes *nodes*)))
      (t (funcall (gethash no *nodes*)))))
#'(lambda () conts))))

(defnode 'people "Is the person a man?" 'male 'femail)

(defnode 'male "Is he living?" 'liveman 'deadman)

(defnode 'deadman "Was he American?" 'us 'them)

(defnode 'us "Is he on a coin?" 'coin 'cidence)

(defnode 'coin "Is the coin a penny?" 'penny 'coins)

(defnode 'penny 'lincoln)

(print (funcall (gethash 'people *nodes*)))


(print (run-tests))

