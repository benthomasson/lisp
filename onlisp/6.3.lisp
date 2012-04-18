
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(defvar *nodes* nil) 

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (format t "~%compile-net ~a: ~a" root node)
    (if (null node)
      nil
      (let ((conts (second node))
            (yes (third node))
            (no (fourth node)))
        (format t "~%conts: ~a yes: ~a no: ~a" conts yes no)
        (if yes
          (let ((yes-fn (compile-net yes))
                (no-fn (compile-net no)))
            #'(lambda ()
                (format t "~A~%>> " conts)
                (funcall (if (eq (read) 'yes)
                           yes-fn
                           no-fn))))
            #'(lambda () conts))))))

        
(defnode 'people "Is the person a man?" 'male 'female)

(defnode 'male "Is he living?" 'liveman 'deadman)

(defnode 'deadman "Was he American?" 'us 'them)

(defnode 'us "Is he on a coin?" 'coin 'cidence)

(defnode 'coin "Is the coin a penny?" 'penny 'coins)

(defnode 'penny 'lincoln)

(print *nodes*)

(compile 'compile-net)


(setf n (compile-net 'people))
(setf *nodes* nil)
(print *nodes*)
(print n)
(print (funcall n))










(print (run-tests))

