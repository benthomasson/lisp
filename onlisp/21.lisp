
(load "/Users/ben/.clisprc.lisp")
(use-package :util)
(asdf-load :unit-test)

(use-package :unit-test)

(defstruct proc pri state wait)

(proclaim '(special *procs* *proc*))

(defvar *halt* (gensym))

(defvar *default-proc*
  (make-proc :state #'(lambda (x) 
                        (format t "~%>> ")
                        (princ (eval (read)))
                        (pick-process))))

(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc 
             :state #'(lambda (,(gensym))
                       ,expr
                       (pick-process))
             :pri ,pri)
           *procs*)))

(defmacro program (name args &body body)
  `(=defun ,name ,args
           (setq *procs* nil)
           ,@body
           (catch *halt* (loop (pick-process)))))


(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
          *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
        (if (> pri max)
          (let ((val (or (not (proc-wait p))
                         (funcall (proc-wait p)))))
            (when val
              (setq proc1 p
                    max pri
                    val1 val))))))
    (values proc1 val1)))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
        (proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))

(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test)
               #'(lambda (,parm) ,@body)))

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))

(defun setpri (n) (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun my-kill (&optional obj &rest args)
  (if obj
    (setq *procs* (apply #'delete obj *procs* args))
    (pick-process)))



(defvar *open-doors* nil)

(=defun pedestrian ()
        (wait d (car *open-doors*)
              (format t "Entering ~A~%" d)))

(program ped ()
         (fork (pedestrian) 1))

;(ped)

(defvar *bboard* nil)

(defun claim (&rest f) (push f *bboard*))

(defun unclaim (&rest f) (pull f *bboard* :test #'equal))

(defun check (&rest f) (find f *bboard* :test #'equal))

(=defun visitor (door)
        (format t "Approach ~A. " door)
        (claim 'knock door)
        (wait d (check 'open door)
              (format t "Enter ~A. " door)
              (unclaim 'knock door)
              (claim 'inside door)))


(=defun host (door)
        (wait k (check 'knock door)
              (format t "Open ~A. " door)
              (claim 'open door)
              (wait g (check 'inside door)
                    (format t "Close ~A.~%" door)
                    (unclaim 'open door))))

(program ballet ()
         (fork (visitor 'door1) 1)
         (fork (host 'door1) 1)
         (fork (visitor 'door2) 1)
         (fork (host 'door2) 1))

;(ballet)


(=defun capture (city)
        (take city)
        (setpri 1)
        (yield
          (fortify city)))

(=defun plunder (city)
        (loot city)
        (ransom city))


(defun take (c) (format t "Liberating ~A.~%" c))
(defun fortify (c) (format t "Rebuilding ~A.~%" c))
(defun loot (c) (format t "Nationalizing ~A.~%" c))
(defun ransom (c) (format t "Refinancing ~A.~%" c))

(program barbarians ()
         (fork (capture 'rome) 100)
         (fork (plunder 'rome) 98))


;(barbarians)


