
(load 'unit-test.lisp)

(defun sentence () ( append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article () (one-of '(the a)))
(defun Noun () (one-of '(man ball woman table)))
(defun Verb () (one-of '(hit took saw liked)))

(defun one-of (set)
    (list (random-elt set)))

(defun random-elt (choices)
    (elt choices (random (length choices))))

(print (sentence))
(print (sentence))
(print (sentence))
(print (sentence))

;;(trace sentence noun-phrase verb-phrase article noun verb)

(print (sentence))


(defun Adj* () 
    (if (= (random 2) 0)
        nil
        (append (Adj) (Adj*))))

(defun PP* ()
    (if (random-elt '(t nil))
        (append (PP) (PP*))
        nil))

 
 (defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
 (defun PP () (append (Prep) (noun-phrase)))
 (defun Adj () (one-of '(big little blue green adiabatic)))
 (defun Prep () (one-of '(to in by with on)))

(trace sentence noun-phrase verb-phrase article noun verb Adj* PP*)

(print (sentence))
(print (sentence))
(print (sentence))

