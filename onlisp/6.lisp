
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)



(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

(defnode 'people "Is the person a man?" 'male 'femail)

(defnode 'male "Is he living?" 'liveman 'deadman)

(defnode 'deadman "Was he American?" 'us 'them)

(defnode 'us "Is he on a coin?" 'coin 'cidence)

(defnode 'coin "Is the coin a penny?" 'penny 'coins)

(defnode 'penny 'lincoln)

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
          (format t "~A~%>>" (node-contents n))
          (case (read)
            (yes (run-node (node-yes n)))
            (t (run-node (node-no n)))))
          (t (node-contents n)))))


(print (run-node 'people))










(print (run-tests))

