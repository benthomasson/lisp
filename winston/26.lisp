

(defpackage :winston26
  (:use :cl :util :unit-test :winston25 :winston24))

(in-package :winston26)

(defparameter *assertions* 'empty-stream)
(defparameter *rules* 'empty-stream)

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun rule-name (rule)
  (first rule))

(defun rule-ifs (rule)
  (butlast (rest rule)))

(defun rule-then (rule)
  (first (last rule)))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(defun try-assertion (pattern assertion bindings)
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
	'empty-stream
	(stream-cons result 'empty-stream))))

(defun match-pattern-to-assertions (pattern bindings)
  (stream-concatenate
   (stream-transform
    #'(lambda (assertion) 
	(try-assertion pattern assertion bindings))
    *assertions*)))

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
   (stream-transform
    #'(lambda (bindings)
	(match-pattern-to-assertions pattern bindings))
    stream)))

(defun apply-filters (patterns initial-input-stream)
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
		     (filter-binding-stream (first patterns)
					    initial-input-stream))))

(defun instantiate-variables (pattern a-list)
  (cond
    ((atom pattern) pattern)
    ((eq '? (first pattern))
     (extract-value (find-binding pattern a-list)))
    (t (cons (instantiate-variables (first pattern) a-list)
	     (instantiate-variables (rest pattern) a-list)))))


(defun use-rule (rule)
  (let ((binding-stream
	 (apply-filters (rule-ifs rule)
			(stream-cons nil 'empty-stream))))
    (do ((binding-stream binding-stream (stream-rest binding-stream))
	 (success-switch nil))
	((stream-endp binding-stream) success-switch)
      (let ((result (instantiate-variables
		     (rule-then rule)
		     (stream-first binding-stream))))
	(when (remember-assertion result)
	  (format t "~%Rule ~a indicates ~a."
		  (rule-name rule) result)
	  (setf success-switch t))))))


(defun forward-chain ()
  (do ((rule-stream *rules* (stream-rest rule-stream))
       (repeat-switch nil))
      ((stream-endp rule-stream)
       (if repeat-switch
	   (progn
	     (format t "~%I am trying the rules again.")
	     (forward-chain))
	   (progn
	     (format t "~%Nothing new noted.")
	     'done)))
    (when (use-rule (stream-first rule-stream))
      (setf repeat-switch t))))

(remember-assertion '(bozo is a dog))
(remember-assertion '(deedee is a horse))
(remember-assertion '(deedee is a parent of sugar))
(remember-assertion '(deedee is a parent of brassy))

(remember-rule
 '(identify
   ((? animal) is a (? species))
   ((? animal) is a parent of (? child))
   ((? child) is a (? species))))









(print (run-tests))
