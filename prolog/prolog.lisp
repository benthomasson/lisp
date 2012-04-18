
(load "/Users/ben/.clisprc.lisp")

(use-package :util)

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
  (let ((binding (get-binding var binding)))
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

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(print-code
  (unify '(?x + 1) '(2 + ?y))
  (unify '?x '?y)
  (unify 'a 'a)
  (unify '(?x ?y a) '(?y ?x ?x))
  (unify '(?x ?x) '(?y ?y))
  (unify '(?x ?x) '(?y ?y)))

(defun unifier (x y)
  "Return somethign that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))


(print-code
  (unifier '(?x ?y a) '(?y ?x ?x))
  (unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
           '(?z + (4 * 5) + 3)))



(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))

(defun get-clauses (pred)
  (get pred 'clauses))

(defun predicate (relation)
  (first relation))

(defvar *db-predicates* nil "A list of all predicates stored in the database.")

(defmacro <- (&rest clause)
  "Add a clause to the database."
  `(add-clause ',clause))


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


(defun prove (goal bindings)
  "Return a list of possible solutions to goal."
  (mapcan #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all (clause-body new-clause)
                           (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal))))

(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) (list bindings))
        (t (mapcan #'(lambda (goal1-solution)
                       (prove-all (rest goals) goal1-solution))
                   (prove (first goals) bindings)))))

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


(defmacro ?- (&rest goals)
  `(prove-all ',goals no-bindings))

(print-code
  (<- (likes Kim Robin))
  (<- (likes Sandy Lee))
  (<- (likes Sandy Kim))
  (<- (likes Robin cats))
  (<- (likes Sandy ?x) (likes ?x cats))
  (<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
  (<- (likes ?x ?x))
  (?- (likes Sandy ?who)))

(defmacro ?-- (&rest goals)
  `(top-level-prove ',goals))

(defun top-level-prove (goals)
  "Prove the goals, and print variables readbly."
  (show-prolog-solutions
    (variables-in goals)
    (prove-all goals no-bindings)))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions."
  (if (null solutions)
    (format t "~%No.")
    (mapc #'(lambda (solution) 
              (show-prolog-vars vars solution))
          solutions))
  (values))

(defun show-prolog-vars (vars bindings)
  "Print each variable with its binding."
  (if (null vars)
    (format t "~%Yes")
    (dolist (var vars)
      (format t "~&~a = ~a" var (subst-bindings bindings var))))
  (princ ";"))

(print-code 
  (?-- (likes Sandy ?who))
  (?-- (likes ?who Sandy))
  (?-- (likes Robin Lee))
  (?-- (likes Sandy cats))
  (?-- (likes ?x ?y) (likes ?y ?x)))
                           

