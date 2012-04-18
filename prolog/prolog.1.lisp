
(defpackage :prolog
  (:use :cl 
        :util)
  (:export :unify 
           :subst-bindings))

(in-package :prolog)

(proclaim '(inline reuse-cons))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (equal x (car x-y)) (eql y (cdr x-y)))
    x-y
    (cons x y)))

(defconstant fail nil "Indicated pat-match failure")

(defconstant no-bindings '((t . t))
             "Indicates pat-match sucess, with no variables.")

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;;Once we add a "real binding,
        ;;we can get rid of the dummy no-bindings
        (if (and (eq bindings no-bindings))
          nil
          bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((eql x y) bindings)
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))

(defun insert-p (x)
  (and (symbolp x) (equal (symbol-name x) "^")))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((and (listp x) (insert-p (car x))) (append (subst-bindings bindings (car (cdr x)))
                                                  (subst-bindings bindings (cdr (cdr x)))))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))


(defun unifier (x y)
  "Return somethign that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))





(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))

(defun get-clauses (pred)
  (get pred 'clauses))

(defun predicate (relation)
  (first relation))

(defvar *db-predicates* nil "A list of all predicates stored in the database.")


(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (reuse-cons (replace-?-vars (first exp))
                       (replace-?-vars (rest exp))
                       exp))))

(defmacro <- (&rest clause)
  "Add a clause to the database."
  `(add-clause ',(replace-?-vars clause)))

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;;The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))


(defun clear-db ()
  "Remvoe all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clause for a single predicate."
  (setf (get predicate 'clauses) nil))


;;;insert here

(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))


(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
      (some 
        #'(lambda (clause)
            (let ((new-clause (rename-variables clause)))
              (prove-all
                (append (clause-body new-clause) other-goals)
                (unify goal (clause-head new-clause) bindings))))
        clauses)
      ;;The predicate's "clauses" can be an atom:
      ;;a primitive function to call
      (funcall clauses (rest goal) bindings other-goals))))



(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) 
                      (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
    (if (funcall predicate tree)
      (adjoin tree found-so-far)
      found-so-far)
    (unique-find-anywhere-if
      predicate
      (first tree)
      (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

(defmacro ?- (&rest goals)
  `(progn
     (print ',goals)
     (top-level-prove ',(replace-?-vars goals))))


(defun top-level-prove (goals)
  (prove-all 
    `(,@goals (show-prolog-vars ,@(variables-in goals)))
    no-bindings)
  (format t "~%No.")
  (values))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions."
  (if (null solutions)
    (format t "~%No.")
    (mapc #'(lambda (solution) 
              (show-prolog-vars vars solution nil))
          solutions))
  (values))

(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
    (format t "~%Yes")
    (dolist (var vars)
      (format t "~&~a = ~a" var
              (subst-bindings bindings var))))
  (if (continue-p)
    fail
    (prove-all other-goals bindings)))

(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)


(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
      (format t "Type ; to see more or . to stop")
      (continue-p))))

;(?- (likes Sandy ?who))
;(?- (likes ?who Sandy))
;(?- (likes Robin Lee))
;(?- (likes Sandy cats))
;(?- (likes ?x ?y) (likes ?y ?x))
;(?- (member ?item ?list))

;(<- (length () 0))
;(<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))

;(<- (member ?item (?item . ?rest)))
;(<- (member ?item (?x . ?rest)) (member ?item ?rest))

;(<- (member ?item (?item . ?)))
;(<- (member ?item (? . ?rest)) (member ?item ?rest))

;(?- (member 2 (1 2 3)))
;(?- (member 2 (1 2 3 2 1)))
;(?- (member ?x (1 2 3)))
                           
;(?- (length ?list ?n))
;(?- (length ?l (1+ (1+ 0))) (member a ?l))
;(?- (member a ?l) (length ?l (1+ (1+ 0))))

