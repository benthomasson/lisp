

(defpackage :winston27
  (:use :cl :util :unit-test :winston24 :winston25))

(in-package :winston27)


(defparameter *assertions* 'empty-stream)
(defparameter *rules* 'empty-stream)

(defun rule-name (rule)
  (first rule))

(defun rule-ifs (rule)
  (butlast (rest rule)))

(defun rule-then (rule)
  (first (last rule)))

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun remember-rule (rule)
  (stream-remember rule *rules*))


(defun apply-filters (patterns initial-input-stream)
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
		     (filter-binding-stream (first patterns)
					    initial-input-stream))))

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
   (stream-transform
    #'(lambda (bindings)
	(stream-concatenate
	 (stream-cons
	  (match-pattern-to-assertions pattern bindings)
	  (stream-cons
	   (match-pattern-to-rules pattern bindings)
	   'empty-stream))))
    stream)))

(defun match-pattern-to-assertions (pattern bindings)
  (stream-concatenate
   (stream-transform
    #'(lambda (assertion) 
	(try-assertion pattern assertion bindings))
    *assertions*)))

(defun try-assertion (pattern assertion bindings)
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
	'empty-stream
	(stream-cons result 'empty-stream))))

(defun match-pattern-to-rules (pattern bindings)
  (stream-concatenate
   (stream-transform
    #'(lambda (rule) 
	(try-rule pattern rule bindings))
    *rules*)))

(defun try-rule (pattern rule bindings)
  (let* ((rule (make-variables-unique rule))
	 (result (unify pattern (rule-then rule) bindings)))
    (if (eq 'fail result)
    'empty-stream
    (apply-filters (rule-ifs rule)
		   (stream-cons result 'empty-stream)))))

(defun make-variables-unique (rule)
  (let ((variables (list-variables rule)))
    (dolist (variable variables rule)
      (setf rule 
	    (instantiate-variables
	     rule
	     (list (list variable 
			 (list '? (gentemp (symbol-name variable))))))))))


(defun list-variables (tree &optional names)
  (cond 
    ((atom tree) names)
    ((eq '? (first tree))
     (if (member (second tree) names)
	 names
	 (append names (rest tree))))
    (t (list-variables (rest tree)
		       (list-variables (first tree) names)))))


(defun instantiate-variables (pattern a-list)
  (cond
    ((atom pattern) pattern)
    ((eq '? (first pattern))
     (let ((binding (find-binding pattern a-list)))
       (if binding
	   (instantiate-variables (extract-variable binding) a-list)
	   pattern)))
    (t (cons (instantiate-variables (first pattern) a-list)
	     (instantiate-variables (rest pattern) a-list)))))


(defun backward-chain (&rest patterns)
  (let ((binding-stream
	 (apply-filters patterns
			(stream-cons nil 'empty-stream)))
	(variables (list-variables patterns))
	(displayed-answers nil))
    (if (endp variables)
	(if (stream-endp binding-stream)
	    'no
	    'yes)
	(do ((binding-stream binding-stream
			     (stream-rest binding-stream)))
	    ((stream-endp binding-stream) 'no-more)
	  (let ((answer (make-answer variables (stream-first binding-stream))))
	    (unless (member answer displayed-answers :test #'equal)
	      (display-answer answer)
	      (setf display-answer (cons answer displayed-answers))))))))

(defun make-answer (variables bindings)
  (instantiate-variables (mapcar #'(lambda (variable)
				     (list variable (list '? variable)))
				 variables)
			 bindings))

(defun display-answer (answers)
  (format t "~&-->")
  (dolist (answer answers)
    (format t " ~a = ~a" (first answer) (second answer))))





(remember-assertion '(deedee has hair))
(remember-assertion '(bozo is a mammal))

(remember-rule '(identity
		 ((? animal) has hair)
		 ((? animal) is a mammal)))


(print (run-tests))