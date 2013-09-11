
(defun derivplus (expr var)
	;(list '+  (deriv (second expr) var)  (deriv (third expr) var))
	(splus (deriv (second expr) var)  (deriv (third expr) var))
)

(defun derivmult (expr var)
	;(list '+  (* (third expr) (deriv (second expr) var))  (* (second expr) (deriv (third expr) var)))
	(splus  (smult (third expr) (deriv (second expr) var))  (smult (second expr) (deriv (third expr) var)))
)

(defun derivdiv (expr var)

	;(list '/ (ssub (smult (third expr) (deriv (second expr) var)) (smult (second expr) (deriv (third expr) var))) (list '* (third expr) (third expr)))
	(sdiv (ssub (smult (third expr) (deriv (second expr) var)) (smult (second expr) (deriv (third expr) var))) (smult (third expr) (third expr)))
)

(defun derivsub (expr var)

	(ssub (deriv (second expr) var)  (deriv (third expr) var))
)

(defun derivunary (expr var)

	(sunary (deriv (second expr) var))
)

(defun deriv (expr var)
 	(if (atom expr)
		  (if (equal expr var) 1 0)
 		  (cond
			   ((eq '+ (first expr)) ; PLUS
				    (derivplus expr var))
  			   ((eq '* (first expr)) ; MULT
				    (derivmult expr var))
  			   ((eq '- (first expr)) ; SUB
				    (if(subsetp (butlast expr) (list '-) )
					(derivunary expr var)	
				    	(derivsub expr var)))
  			   ((eq '/ (first expr)) ; DIV
				    (derivdiv expr var))
  			   (t ; Invalid
		       		  (error "Invalid Expression!"))
 		    )
	 )
)

(defun splus (x y)
	(if (numberp x)
		(if (numberp y)
			(+ x y)
			(if (zerop x)
				y
				(list '+ x y)
			)
		)
		(if (and (numberp y) (zerop y))
			x
			(list '+ x y)
		)
	)
)

(defun sunary (x)
	(if (numberp x)
		(* -1 x)
		(list '- x)
	)
)

(defun ssub (x y)
	(if (numberp x)
		(if (numberp y)
			(- x y)
			(if (zerop x)
				(sunary y)
				(list '- x y)
			)
		)
		(if (and (numberp y) (zerop y))
			x
			(list '- x y)
		)
	)
)

(defun smult (x y)
	(if (numberp x)
		(if (numberp y)
			(* x y)
			(if (zerop x)
				0
				(if (eq x 1)
				  y
				  (if(eq x -1)
					(sunary y)
				  	(list '* x y)
				   )
				)
			)
		)
		(if(numberp y)
			(if(zerop y)
		;(if (and (numberp y) (zerop y))
				0
				(if (eq y 1)
					x
					(if(eq y -1)
						(sunary x)
						(list '* x y)
					)
				)		
			)	
		)
	)
)

(defun sdiv (x y)
	(if (equalp 0 y)
            (print "Error")
	    (if (equalp 0 x)
		0
		(if (equal x y)
			1
			(if (and (numberp x) (numberp y))
				(/ x y)
				 (list '/ x y)
			)
		)
	     )
	)
)

			
				
				
	
