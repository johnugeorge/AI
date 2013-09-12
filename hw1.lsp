
(defun derivplus (expr var)
	(splus (deriv (second expr) var)  (deriv (third expr) var))
)

(defun derivmult (expr var)
	(splus  (smult (third expr) (deriv (second expr) var))  (smult (second expr) (deriv (third expr) var)))
)

(defun derivdiv (expr var)

	(sdiv (ssub (smult (third expr) (deriv (second expr) var)) (smult (second expr) (deriv (third expr) var))) (smult (third expr) (third expr)))
)

(defun derivsub (expr var)

	(ssub (deriv (second expr) var)  (deriv (third expr) var))
)

(defun derivunary (expr var)

	(sunary (deriv (second expr) var))
)

(defun derivexpt (expr var)
	
	(smult (third expr) (smult (sexpt (second expr) (ssub (third expr) 1)) (deriv (second expr) var)))
)

(defun derivsqrt (expr var)
	
	(smult (sdiv 1 2) (smult (sexp (second expr) (ssub (sdiv 1 2) 1)) (deriv (second expr) var)))
)

(defun derivlog (expr var)
	
	(smult (sdiv 1 (second expr)) (deriv (second expr) var))
)

(defun derivexp (expr var)
	
	(smult (sexp expr) (deriv (second expr) var))
)

(defun deriv (expr var)
 	(if (atom expr)
		  (if (equal expr var) 1 0)
 		  (cond
			   ((eq '+ (first expr)) ; PLUS
				    (derivplus expr var))
  			   ((eq '* (first expr)) ; MULT
				    (derivmult expr var))
  			   ((eq '- (first expr)) 
				    (if(subsetp (butlast expr) (list '-) )
					(derivunary expr var) ; UNARY
				    	(derivsub expr var))) ;SUB
  			   ((eq '/ (first expr)) ; DIV
				    (derivdiv expr var))
  			   ((eq 'expt (first expr)) ; EXP
				    (derivexpt expr var))
  			   ((eq 'sqrt (first expr)) ; sqrt
				    (derivsqrt expr var))
  			   ((eq 'log (first expr)) ; log
				    (derivlog expr var))
  			   ((eq 'exp (first expr)) ; power e
				    (derivexp expr var))
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
			(if(equal x y)
				0
				(list '- x y)
			)
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
				0
				(if (eq y 1)
					x
					(if(eq y -1)
						(sunary x)
						(list '* x y)
					)
				)		
			)	
			(list '* x y)
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

(defun sexpt (x y)
	(if (and (numberp x) (numberp y))
		(expt x y)
		(if (and  (numberp y) (eq y 0))
			1
			(if (and (numberp y)(eq y 1))
				x
				(list 'expt x y)
			)
		)
	)
)				
			
(defun sexp (x)
	x
)	
				
	
