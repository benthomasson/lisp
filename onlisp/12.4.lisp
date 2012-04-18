
(load "../util/util.lisp")
(use-package :util)

(load "../unit-test/unit-test.lisp")
(use-package :unit-test)

(print-code
  (setf x 1)
  (setf y 3)
  (setf x (+ x y))
  (get-setf-method '(aref a (incf i))))


(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
    (get-setf-method place)
    `(let (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(print '_f)

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
    (get-setf-method place)
    (let ((g (gensym)))
          `(let* ((,g ,obj)
                  ,@(mapcar #'list vars forms)
                  (,(car var) (delete ,g ,access ,@args)))
             ,set))))

(print 'pull)

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
    (get-setf-method place)
    (let ((g (gensym)))
          `(let* ((,g ,test)
          ,@(mapcar #'list vars forms)
          (,(car var) (delete-if ,g ,access ,@args)))
      ,set))))

(print 'pull-if)

(defmacro popn (n place)
  (multiple-value-bind (vars forms vars set access)
    (get-setf-method place)
    (util:with-gensyms (gn glst)
                       `(let* ((,gn ,n)
                               ,@(mapcar #'list vars forms)
                               (,glst ,access)
                               (,(car var) (nthcdr ,gn ,glst)))
                          (prog1 (subseq ,glst 0 ,gn)
                            ,set)))))

(print 'popn)







(print (run-tests))
