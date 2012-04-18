
(load "prolog.1.lisp")


(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (= ?x ?x))
(setf (get '!= 'clauses) '!=)

(<- (female amy))
(<- (female (amy)))
(<- (female (johnette)))
(<- (parentof (amy johnette)))
(<- (parentof (amy bob)))
(<- (childof (?x ?y)) (parentof (?y ?x)))
(<- (siblingof (?x ?y)) 
    (parentof (?z ?x))
    (parentof (?z ?y)))


;(?- (member ?x (1 2 3)))
;(?- (member ?item ?list))

;(?- (female amy))
;(?- (female (?x)))
;(?- (parentof (?y ?x)))
;(?- (childof (?y ?x)))
;(?- (siblingof (johnette ?x)))
(?- (!= 4 5))
(?- (!= 5 5))





