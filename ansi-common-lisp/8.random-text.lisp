
(load "../unit-test/unit-test.lisp")

(use-package :unit-test)

(setf *random-state* (make-random-state t))

(defparameter *words* (make-hash-table :size 10000))
(defparameter *second-words* (make-hash-table :size 10000))
(defparameter *third-words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
          (pos 0))
      (do ((c (read-char s nil :eof) (read-char s nil :eof)))
        ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
          (progn 
            (setf (aref buffer pos) c)
            (incf pos))
          (progn
            (unless (zerop pos)
              (see (intern (string-downcase
                             (subseq buffer 0 pos))))
              (setf pos 0))
            (let ((p (punc c)))
              (if p (see p)))))))))

(defun punc (c)
  (case c
    (#\. '|.|)
    (#\, '|,|)
    (#\; '|;|)
    (#\! '|!|)
    (#\? '|?|)))

(deftest test-punc
         (assert-equal (punc #\.) '|.|)
         (assert-equal (punc #\,) '|,|)
         (assert-equal (punc #\;) '|;|)
         (assert-equal (punc #\!) '|!|)
         (assert-equal (punc #\?) '|?|))

(let ((prev '|.|)
      (prev-prev '|.|)
      (prev-prev-prev '|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
        (push (cons symb 1) (gethash prev *words*))
        (incf (cdr pair))))
    (let ((pair (assoc symb (gethash prev-prev *second-words*))))
      (if (null pair)
        (push (cons symb 1) (gethash prev-prev *second-words*))
        (incf (cdr pair))))
    (let ((pair (assoc symb (gethash prev-prev *third-words*))))
      (if (null pair)
        (push (cons symb 1) (gethash prev-prev *third-words*))
        (incf (cdr pair))))
    ;;(format t "~%see: ~a prev: ~a prev-prev: ~a" symb prev prev-prev)
    ;;(format t "~%~a words: ~a" prev (gethash prev *words*))
    ;;(format t "~%~a second-words: ~a" prev (gethash prev *second-words*))
    (setf prev-prev-prev prev-prev)
    (setf prev-prev prev)
    (setf prev symb)))

#|(deftest test-see
         (let ((*words* (make-hash-table :size 10))
               (*second-words* (make-hash-table :size 10)))
           (assert-equal (see 'abc) 'abc)
           (assert-equal (gethash '|.| *words*) '((abc . 1)))
           (assert-equal (gethash '|.| *second-words*) '((|.| . 1)))
           (assert-equal (see 'abc) 'abc)
           (assert-equal (gethash '|.| *words*) '((abc . 1)))
           (assert-equal (gethash 'abc *words*) '((abc . 1)))
           (assert-equal (gethash '|.| *second-words*) '((abc . 1) (|.| . 1)))
           (see '|.|)
           (see 'a)
           (see '|.|)
           (see 'b)
           (see '|.|)
           (see 'c)
           (let* ((choices (gethash '|.| *words*))
             (i (random (reduce #'+ choices :key #'cdr))))
             (format t "~a ~a ~a" choices i (reduce #'+ choices :key #'cdr))))
         (assert-equal (gethash '|.| *words* ) nil)
         (assert-equal (gethash 'abc *words* ) nil))|#

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
    (terpri)
    (let ((next (random-next prev)))
      (format t "~A " next)
      (generate-text (1- n) next))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
        (return (car pair))))))

(let ((prev-prev '|.|))
(defun random-next-by-last-two (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
        (return (car pair)))))))

(defun random-next-2 (prev)
  (let* ((choices (gethash prev *second-words*))
         (i (random (reduce #'+ choices :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
        (return (car pair))))))

(defun random-next-3 (prev)
  (let* ((choices (gethash prev *third-words*))
         (i (random (reduce #'+ choices :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
        (return (car pair))))))

(deftest test-random-next
         (let ((*words* (make-hash-table :size 10))
               (*second-words* (make-hash-table :size 10)))
           (see '|.|)
           (see '|.|)
           (see 'a)
           (see 'b)
           (see 'c)
           (print (random-next 'a))
           (print (random-next-2 'a))))

;;(print (run-tests))

(read-text "input.text")

(terpri)

(loop
  (generate-text 100)
  (read))
