
(defpackage :6tools
  (:use :cl :util :unit-test))

(in-package :6tools)

(defvar *dbg-ids* nil )

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defvar *WARN-ON-FLOATING-POINT-CONTAGION* nil)

(defun find-all-if (test list)
  (remove-if-not test list))

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun mydebug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogehter."
  (setf *dbg-ids* (if (null ids) nil
                    (set-difference *dbg-ids* ids))))

(defun dbg-indent (id ident format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids* )
    (fresh-line *debug-io*)
    (dotimes (i ident) (princ "   " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(#+ :ignore (defun lisp ()
  (loop
    (print '>)
    (print (eval (read))))))

(#+ :ignore (defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
    (print prompt)
    (print (funcall transformer (read))))))

(#+ :ignore (defun lisp ()
  (interactive-interpreter ">" #'eval)))

(defun compose (f g)
  "Return the function that computes (f (g x))."
    #'(lambda (x) (funcall f (funcall g x))))

(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
    (handler-case
      (progn
        (if (stringp prompt)
          (format t "~a" prompt)
          (funcall prompt))
        (print (funcall transformer (read))))
      ;;In case of error, do this:
      (error (condition)
             (format t "~&;; Error ~a ignorned, back to top level."
                     condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [1], [2], etc."
    #'(lambda () (format t ctl-string (incf num))))

(defun lisp ()
  (interactive-interpreter (prompt-generator 0 "~d >") #'eval))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern  against input in the context of the bindings."
  (cond ((eq bindings fail) fail)
       ((variable-p pattern)
        (match-variable pattern input bindings))
       ((eql pattern input) bindings)
       ((segment-pattern-p pattern)
        (segment-matcher pattern input bindings))
       ((single-pattern-p pattern)
        (single-matcher pattern input bindings))
       ((and (consp pattern) (consp input))
        (pat-match (rest pattern) (rest input)
                         (pat-match (first pattern) (first input)
                                    bindings)))
        (t fail)))


(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(deftest test-constants
    (assert-equal fail nil)
    (assert-equal no-bindings (list (cons t t))))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(deftest test-variable-p
        (assert-true (variable-p '?x))
        (assert-true (not (variable-p 'x))))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(deftest test-get-binding
           (assert-equal (get-binding '?x '((?X . 5) (?Y . 6))) '(?X . 5))
           (assert-equal (get-binding '?y '((?X . 5) (?Y . 6))) '(?y . 6))
           (assert-equal (get-binding '?z '((?X . 5) (?Y . 6))) nil))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(deftest test-binding-var
    (assert-equal (binding-var '(?x . 5)) '?x))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(deftest test-binding-val
    (assert-equal (binding-val '(?x . 5)) 5))

(defun make-binding (var val)
  (cons var val))

(deftest test-make-binding
    (assert-equal (make-binding '?x 5) '(?x . 5)))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(deftest test-lookup
    (assert-equal (lookup '?x '((?x . 5))) 5)
    (assert-equal (lookup '?y '((?x . 5))) nil))

(defun extend-bindings (var val bindings)
  "Add a ( var . value) pair to a binding list."
  (cons (make-binding var val)
        ;;Once we add a "real" binding,
        ;;we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
          nil
          bindings)))

(deftest test-extend-bindings
           (assert-equal (extend-bindings '?x 5 no-bindings) '((?x . 5)))
           (assert-equal (extend-bindings '?x 5 '((?y . 6))) '((?x . 5) (?y . 6)))
           (assert-equal (extend-bindings '?x 5 nil) '((?x . 5))))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(deftest test-match-variable
           (assert-equal (match-variable '?x 5 no-bindings) '((?x . 5)))
           (assert-equal (match-variable '?x 5 '((?x . 5))) '((?x . 5)))
           (assert-equal (match-variable '?x 6 '((?x . 5))) fail))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern ) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-match pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(deftest test-segment-pattern-p
           (assert-equal (segment-pattern-p '((?* ?x) . 5)) 'segment-match)
           (assert-equal (segment-pattern-p '((?+ ?x) . 5)) 'segment-match+)
           (assert-equal (segment-pattern-p '((?if ?x) . 5)) 'match-if)
           (assert-equal (segment-pattern-p '((?? ?x) . 5)) 'segment-match?))

(deftest test-single-pattern-p
           (assert-equal (single-pattern-p '(?or ?X . 5)) 'match-or)
           (assert-equal (single-pattern-p '(?is ?X . 5)) 'match-is)
           (assert-equal (single-pattern-p '(?not ?X . 5)) 'match-not)
           (assert-equal (single-pattern-p '(?and ?X . 5)) 'match-and))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
      fail
      new-bindings)))

(deftest test-match-is
           (assert-equal (match-is '(?x oddp) 3 no-bindings) '((?X . 3)))
           (assert-equal (match-is '(?x evenp) 3 no-bindings) nil))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(deftest test-match-and
;;(print-code (match-and nil nil no-bindings))
           (assert-equal (match-and nil nil no-bindings) '((t . t)))
           (assert-equal (match-and '(?x) 5 no-bindings) '((?x . 5)))
           (assert-equal (match-and '((?x)) '(5) no-bindings) '((?x . 5)))
           (assert-equal (match-and '((?x 6) (?y 6)) '(5 6) no-bindings) '((?y . 5) (?x . 5))))

;;(print-code (match-and '((?x)) '(5) no-bindings))
;;(print-code (match-and '((?x 6) (?y 6)) '(5 6) no-bindings))


(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
    fail
    (let ((new-bindings (pat-match (first patterns) input bindings)))
          (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(deftest test-match-or
;;(print-code (match-or nil nil no-bindings))
;;(print-code (match-or '(?x) 5 no-bindings))
           (assert-equal (match-or nil nil no-bindings) nil)
           (assert-equal (match-or '(?x) 5 no-bindings) '((?x . 5)))
           (assert-equal (match-or '((?x)) '(5) no-bindings) '((?x . 5)))
           (assert-equal (match-or '((?x 6) (?y 7)) '(5 6) no-bindings) '((?x . 5)))
           (assert-equal (match-or '((?x 6) (?y 7)) '(6 7) no-bindings) '((?y . 6)))
           (assert-equal (match-or '((?x 6) (?y 6)) '(5 6) no-bindings) '((?x . 5))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any varaibles."
  (if (match-or patterns input bindings)
    fail
    bindings))

(deftest test-match-not
;;When does match-not fail?
;;Is this correct?
           (assert-equal (match-not nil nil no-bindings) '((t . t)))
           (assert-equal (match-not '(a b c) '(a b c) no-bindings) '((t . t)))
           (assert-equal (match-not '(a) '(a) no-bindings) '((t . t)))
           (assert-equal (match-not '(a) '(b) no-bindings) '((t . t)))
           (assert-equal (match-not '(c) '(b) no-bindings) '((t . t))))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      (let ((pos (first-match-pos (first pat) input start)))
        (if (null pos)
          fail
          (let ((b2 (pat-match 
                      pat (subseq input pos)
                      (match-variable var (subseq input 0 pos)
                                      bindings))))
            ;;If this match failed, try another longer one
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possible match input,
  starting at position start. If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((< start (length input)) start)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
   "Math zero or one element of input."
   (let ((var (second (first pattern)))
         (pat (rest pattern)))
     (or (pat-match (cons var pat) input bindings)
         (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  (and (progv (mapcar #'car bindings)
              (mapcar #'cdr bindings)
              (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(deftest test-pat-match
;;(print-code (pat-match nil nil))
           (assert-equal (pat-match nil nil) '((t . t)))
           (assert-equal (pat-match '(a b c) '(a b c)) '((t . t)))
           (assert-equal (pat-match '(a b c) '(a b c d)) nil)
           (assert-equal (pat-match '(a b c d) '(a b c)) nil)
           (assert-equal (pat-match '(a b c ?x) '(a b c)) nil)
           (assert-equal (pat-match '(a b c ?x) '(a b c d)) '((?x . d)))
           (assert-equal (pat-match '(a (?* ?x) d) '(a b c d)) '((?x b c)))
           (assert-equal (pat-match '(a (?* ?x) (?* ?y) d) '(a b c d)) '((?y b c) (?x)))
           (assert-equal (pat-match '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d))) '((?y d) (?x b c))))


;;Hmmm seems to be a bug here.
;;(setf code '(?z ?y ?op ?x))
;;(progv '(?z ?y ?op ?x) '(7 4 + 3) (print (eval 'code)))

;;(trace pat-match match-if)
;;(print-code (pat-match '(?x ?op ?y is ?z (?if (eql (?op ?x ?y) ?z)))
;;                       '(3 + 4 is 7)))



(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
        (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern match abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

;;(print-code (cond ((and 'a 'b))))

;;(print-code (pat-match-abbrev '?x* '(?* ?x)))
;;(print-code (pat-match-abbrev '?y* '(?* ?y)))

;;(print-code (expand-pat-match-abbrev '(a ?x* ?y* d)))


;;Search tools


(defun tree-search (states goal-p successors combiner)
  "Find a state that statisfies goal-p. Start with states,
  and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))
  

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

(#+ :ignore (defun is (value) #'(lambda (x) (eql x value))))

(deftest test-binary-tree
           (funcall (is 12) 12)
           (assert-equal (binary-tree 1) '(2 3))
           (assert-equal (binary-tree 10) '(20 21)))

(mydebug :search)

;;(depth-first-search 1 (is 12) #'binary-tree)

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(breadth-first-search 1 (is 12) #'binary-tree)

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))

(deftest test-finite-binary-tree
        (assert-equal (funcall (finite-binary-tree 15) 1) '(2 3))
        (assert-equal (funcall (finite-binary-tree 15) 6) '(12 13))
        (assert-equal (funcall (finite-binary-tree 15) 9) nil))

(depth-first-search 1 (is 12) (finite-binary-tree 15))

(defun diff (num)
  "Return the function that finds the difference from num."
  #'(lambda (x) (abs (- x num))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))


(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(best-first-search 1 (is 12) #'binary-tree (diff 12))

(defun price-is-right (price)
  "Return a function that measures the difference from price,
  but gives a big penalty for a going over pirce."
  #'(lambda (x) (if (> x price)
                  most-positive-fixnum
                  (- price x))))

(best-first-search 1 (is 12) #'binary-tree (price-is-right 12))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  (tree-search (list start) goal-p successors
               #'(lambda (old new)
                   (let ((sorted (funcall (sorter cost-fn) old new)))
                     (if (> beam-width (length sorted))
                       sorted
                       (subseq sorted 0 beam-width))))))


(beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2)


(defstruct (city (:type list)) name long lat)

(defparameter *cities*
'((Atlanta      84.23 33.45) (Los-Angeles   118.15 34.03)
(Boston       71.05 42.21) (Memphis        90.03 35.09)  
(Chicago      87.37 41.50) (New-York       73.58 40.47) 
(Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
(Eugene      123.05 44.03) (Pittsburgh     79.57 40.27) 
(Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
(Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
(Houston     105.00 34.00) (San-Francisco 122.26 37.47)
(Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
(Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
(Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (find-all-if #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities*))

(#+ :ignore (defun trip (start dest)
  "Search for a way from the start to dest."
  (beam-search start (is dest) #'neighbors
               #'(lambda (c) (air-distance c dest))
               1)))

(defstruct (path (:print-function print-path))
          state (previous nil) (cost-so-far 0) (total-cost 0))


(defconstant earth-diameter 12765.0)

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;;d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  "Convert degrees to radians."
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))

(print-code (trip (city 'san-francisco) (city 'boston)))
(print-code (trip (city 'boston) (city 'san-francisco)))

(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a give value."
    #'(lambda (path) (funcall test value (funcall key path))))

(defun path-saver (successors cost-fn cost-left-fn)
    #'(lambda (old-path)
        (let ((old-state (path-state old-path)))
          (mapcar
            #'(lambda (new-state)
                (let ((old-cost
                        (+ (path-cost-so-far old-path)
                           (funcall cost-fn old-state new-state))))
                  (make-path
                    :state new-state
                    :previous old-path
                    :cost-so-far old-cost
                    :total-cost (+ old-cost (funcall cost-left-fn
                                                     new-state)))))
            (funcall successors old-state)))))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))

(defun show-city-path (path &optional (stream t))
  "Show the lenght of a path, and the cities along it."
  (format stream "~&#<Path ~,1f km: ~{~:(~a~)~^ - ~}>"
          (path-total-cost path)
          (reverse (map-path #'city-name path)))
  (values))

(defun map-path (fn path)
  "Call fn on each state in the path, collecting results."
  (if (null path)
    nil 
    (cons (funcall fn (path-state path))
          (map-path fn (path-previous path)))))


(defun trip (start dest &optional (beam-width 1))
  "Search for the best path from the start to dest."
  (beam-search
    (make-path :state start)
    (is dest :key #'path-state)
    (path-saver #'neighbors #'air-distance
                #'(lambda (c) (air-distance c dest)))
    #'path-total-cost
    beam-width))

(undebug)

(show-city-path (trip (city 'san-francisco) (city 'boston) 1))
(show-city-path (trip (city 'boston) (city 'san-francisco) 1))
(show-city-path (trip (city 'boston) (city 'san-francisco) 3))

(defun iter-wide-search (start goal-p successors cost-fn
                               &key (width 1) (max 100))
  "Search, increasing beam width from width to max.
   Return the first solution found at any width."
   (dbg :search "; Width: ~d" width)
   (unless (> width max)
     (or (beam-search start goal-p successors cost-fn width)
         (iter-wide-search start goal-p successors cost-fn
                           :width (+ width 1) :max max))))

(mydebug :search)

(iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12))


(defun graph-search (states goal-p successors combiner
                            &optional (state= #'eql) old-states)
  "Find a state that satifies goal-p. Start with states,
  and search according to successors and combiner.
  Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
             (funcall
               combiner
               (new-states states successors state= old-states)
               (rest states))
             goal-p successors combiner state=
             (adjoin (first states) old-states
                     :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if
    #'(lambda (state)
        (or (member state states :test state=)
            (member state old-states :test state=)))
    (funcall successors (first states))))

(defun next2 (x) (list (+ x 1) (+ x 2)))

(tree-search '(1) (is 6) #'next2 #'prepend)

(graph-search '(1) (is 6) #'next2 #'prepend)

(defun find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  ;;MERGE is a built-in-function
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (if (null path)
    nil
    (cons (path-state path)
          (path-states (path-previous path)))))

;;(defun a*-search (paths goal-p successors cost-fn cost-left-fn &optional (state= #'eql) old-paths)
  ;;"Find a path whose state satisfies goal-p. Start with paths,
  ;;and expand successors, exploring least cost first.
  ;;When there are duplicate states, keep the one with the 
  ;;lower cost and discard the other."
  ;;(dgb :search ";; Search: ~a" paths)
  ;;(cond 
    ;;((null paths) fail)
    ;;((funcall goal-p (path-state (first paths)))
     ;;(values (first paths) paths))
    ;;(t (let* ((path (pop paths))
              ;;(state (path-state path)))
         ;;;;Update PATHS and OLD-PATHS to reflect
         ;;;;the new successors of STATE:
         ;;(setf old-paths (insert-path path old-paths))
         ;;(dolist (state2 (funcall successors state))
           ;;(let* ((cost (+ (path-cost-so-far path)
                           ;;(funcall cost-fn state state2)))
                  ;;(cost (funcall cost-left-fn state2))
                  ;;(path2 (make-path
                           ;;:state state2 :previous path
                           ;;:cost-so-far cost
                           ;;:total-cost (+ cost cost2)))
                  ;;(old nil)
                  ;;;;Place the new path, path2, in the right list:
                  ;;(cond
                    ;;((setf old (find-path state2 paths state=))
                     ;;(when (better-path path2 old)
                       ;;(setf paths (insert-path
                                     ;;path2 (delete old paths)))))
                    ;;((setf old (find-path state2 old-paths state=))
                     ;;(when (better-path path2 old)
                       ;;(setf paths (insert-path path2 paths))
                       ;;(setf old-paths (delete old old-paths))))
                    ;;(t (setf paths (insert-path path2 paths))))))
           ;;Finally, call A* again with the update path lists:
           ;;(a*-search paths goal-p successors cost-fn cost-left cost-left-fn
                      ;;state= old-paths))))))

(defun search-all (start goal-p successors cost-fn beam-width)
  "Find all solutions to a search problem, using beam search."
  ;;Be careful: this can lead to an infinite loop.
  (let ((solutions nil))
    (Beam-search
      start #'(lambda (x)
                (when (funcall goal-p x) (push x solutions))
                nil)
      successors cost-fn beam-width)
    solutions))

(print (run-tests))
