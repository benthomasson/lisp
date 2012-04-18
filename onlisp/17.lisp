

(load "/Users/ben/.clisprc.lisp")

(use-package :util)
(asdf-load :unit-test)

(use-package :unit-test)

(set-dispatch-macro-character #\# #\?
#'(lambda (stream char1 char2)
    `#'(lambda (&rest ,(gensym))
         ,(progn
            (format t "l: ~a ~a" char1 char2)
            (print (read stream t nil t))
            ))))

(print-code 
  (mapcar #?9 '(a b c)))



(print-code (get-macro-character #\)))
(print-code (get-macro-character #\())

(print-code (set-macro-character #\] (get-macro-character #\))))

(set-dispatch-macro-character #\# #\[ 
                              #'(lambda (stream char1 char2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\] stream t)))
                                    (do ((i (ceiling (car pair)) (1+ i)))
                                      ((> i (floor (cadr pair)))
                                       `'(,@(nreverse accum)))
                                      (push i accum)))))


(print-code
#[2 7])


(print-code (set-macro-character #\} (get-macro-character #\))))

(set-macro-character #\{ 
                     #'(lambda (stream char)
                         (print (read-delimited-list #\} stream t))))


(print-code
  {+ 1 1})

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))


(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
                                  #'(lambda (stream char1 char2)
                                      (apply fn (read-delimited-list right stream t))))))

(defdelim #\[ #\] (x y)
          (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

(print-code 
  #[2 10]
  #.(compose #'list #'1+))


(defdelim #\{ #\} (&rest args)
          `(fn (compose ,@args)))

(print-code (funcall #{list 1+} 7))

(print (run-tests))



