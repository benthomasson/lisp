
(defpackage :winston25-3
  (:use :cl :util :unit-test))

(in-package :winston25-3)

(defmacro encapsulate (form)
  `#'(lambda () ,form))

(defmacro expose (procedure)
  `(funcall ,procedure))

(defun stream-endp (stream) (eq stream 'empty-stream))
(defun stream-first (stream) (first stream))
(defun stream-rest (stream) 
  (if (functionp (second stream))
    (setf (second stream) (expose (second stream)))
    (second stream)))

(defmacro stream-cons (object stream)
  `(list ,object (encapsulate ,stream)))

(defun stream-append (stream1 stream2)
  (if (stream-endp stream1)
    stream2
    (stream-cons (stream-first stream1)
                 (stream-append (stream-rest stream1)
                                stream2))))

(deftest test-stream
         (let ((s (stream-cons 'a 'empty-stream)))
           (assert-equal 'a (stream-first s))
           (assert-equal 'empty-stream (stream-rest s))
           (assert-true (stream-endp (stream-rest s)))
           (let ((s2 (stream-cons 'b s)))
             (assert-equal 'b (stream-first s2))
             (let ((s3 (stream-append s s2)))
               (assert-equal 'a (stream-first s3))
               (assert-equal 'b (stream-first (stream-rest s3)))
               (assert-equal 'a (stream-first (stream-rest (stream-rest s3))))
               (assert-true (stream-endp (stream-rest (stream-rest (stream-rest s3)))))))))



(defun stream-concatenate (streams)
  (if (stream-endp streams) 'empty-stream
    (if (stream-endp (stream-first streams))
      (stream-concatenate (stream-rest streams))
      (stream-cons (stream-first (stream-first streams))
                   (stream-concatenate
                     (stream-cons (stream-rest (stream-first streams))
                                  (stream-rest streams)))))))

(deftest test-stream-concatenate
         (let* ((s1 (stream-cons 'a (stream-cons 'b 'empty-stream)))
               (s2 (stream-cons 'x (stream-cons 'y 'empty-stream)))
               (ss (stream-cons s1 (stream-cons s2 'empty-stream)))
               (sa (stream-append s1 s2))
               (sc (stream-concatenate ss)))
           (assert-equal 'a (stream-first sa))
           (assert-equal 'a (stream-first sc))
           (assert-equal 'b (stream-first (stream-rest sa)))
           (assert-equal 'b (stream-first (stream-rest sc)))
           (assert-equal 'x (stream-first (stream-rest (stream-rest sa))))
           (assert-equal 'x (stream-first (stream-rest (stream-rest sa))))))

(defun stream-transform (procedure stream)
  (if (stream-endp stream)
    'empty-stream
    (stream-cons (funcall procedure (stream-first stream))
                 (stream-transform procedure
                                   (stream-rest stream)))))

(deftest test-stream-transform
         (let* ((s (stream-cons 2 (stream-cons 3 'empty-stream)))
                (st (stream-transform #'(lambda (number)
                                          (expt 2 number)) s)))
           (assert-equal 4 (stream-first st))
           (assert-equal 8 (stream-first (stream-rest st)))))

(defun stream-member (object stream)
  (cond ((stream-endp stream) nil)
        ((equal object (stream-first stream)) t)
        (t (stream-member object (stream-rest stream)))))

(deftest test-stream-member 
         (let ((s (stream-cons 2 (stream-cons 3 'empty-stream))))
           (assert-true (stream-member 2 s))
           (assert-true (stream-member 3 s))
           (assert-nil (stream-member 4 s))))

(defmacro stream-remember (object variable)
  `(unless (stream-member ,object ,variable)
     (setf ,variable
           (stream-append ,variable
                          (stream-cons ,object
                                       'empty-stream)))
     ,object))

(deftest test-stream-remember 
         (let ((s 'empty-stream))
           (assert-true (stream-endp s))
           (assert-true (stream-remember 1 s))
           (assert-nil (stream-remember 1 s))
           (assert-equal 2 (stream-remember 2 s))
           (assert-equal 1 (stream-first s))
           (assert-equal 2 (stream-first (stream-rest s)))
           (assert-true (stream-endp (stream-rest (stream-rest s))))))
         
(print (run-tests))
