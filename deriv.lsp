
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
	
	(smult (sdiv 1 2) (smult (sexpt (second expr) (ssub (sdiv 1 2) 1)) (deriv (second expr) var)))
)

(defun derivlog (expr var)
	
	(smult (sdiv 1 (second expr)) (deriv (second expr) var))
)

(defun derivexp (expr var)
	
	(smult (sexp (second expr)) (deriv (second expr) var))
)

(defun derivsin (expr var)
	
	(smult (scos (second expr)) (deriv (second expr) var))
)

(defun derivcos (expr var)
	
	(sunary (smult (ssin (second expr)) (deriv (second expr) var)))
)

(defun derivtan (expr var)
	
	(smult (splus 1 (sexpt (stan (second expr)) 2)) (deriv (second expr) var))
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
  			   ((eq 'sin (first expr)) ; power e
				    (derivsin expr var))
  			   ((eq 'cos (first expr)) ; power e
				    (derivcos expr var))
  			   ((eq 'tan (first expr)) ; power e
				    (derivtan expr var))
  			   (t ; Invalid
		       		  (error "Invalid Expression!"))
 		    )
	 )
)

(defun simplify (expr)
	(if (atom expr)
		expr
		(cond
			   ((eq '+ (first expr)) ; PLUS
				    (splus (simplify (second expr)) (simplify (third expr))))
  			   ((eq '* (first expr)) ; MULT
				    (smult  (simplify (second expr)) (simplify (third expr))))
  			   ((eq '- (first expr)) 
				    (if(subsetp (butlast expr) (list '-) )
					(sunary (simplify (second expr))) ; UNARY
				    	(ssub  (simplify (second expr)) (simplify (third expr))))) ;SUB
  			   ((eq '/ (first expr)) ; DIV
				    (sdiv (simplify (second expr)) (simplify (third expr))))
  			   ((eq 'expt (first expr)) ; EXP
				    (sexpt (simplify (second expr)) (simplify (third expr))))
  			   ((eq 'sqrt (first expr)) ; sqrt
				    (ssqrt (simplify (second expr))))
  			   ((eq 'log (first expr)) ; log
				    (slog (simplify (second expr))))
  			   ((eq 'exp (first expr)) ; power e
				    (sexp (simplify (second expr))))
  			   ((eq 'sin (first expr)) ; power e
				    (ssin (simplify (second expr))))
  			   ((eq 'cos (first expr)) ; power e
				    (scos (simplify (second expr))))
  			   ((eq 'tan (first expr)) ; power e
				    (stan (simplify (second expr))))
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
	(if (eq 0 y)
            (print " Division by Zero. Error !!")
	    (if (eq 0 x)
		0
		(if (equal x y)
			1
			(if (eq y 1)
				x
				(if (eq y -1)
					(sunary x)
					(if (and (numberp x) (numberp y))
						(/ x y)
				 		(list '/ x y)
					)
				)
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
	(if(numberp x)
		(exp x)
		(list 'exp x)
	  )
)	

(defun slog (x)
	(if(and (numberp x) (zerop x))
		(print "Log error as argument is zero" )
		(if (numberp x)
			(log x)
			(list 'exp x)
		)
	  )
)

(defun ssqrt (x)
	(if(and (numberp x) (zerop x))
		(print "sqrt error as argument is zero" )
		(if (numberp x)
			(sqrt x)
			(list 'sqrt x)
		)
	  )
)

(defun ssin (x)	
	(if (numberp x)	
		(sin x)
		(list 'sin x)
	)
)

(defun scos (x)	
	(if (numberp x)
		(cos x)
		(list 'cos x)
	)
)

(defun stan (x)	
	(if (numberp x)
		(tan x)
		(list 'tan x)
	)
)

(defun deriv-eval (expr var val)

	(setq deriv-result (deriv expr var))
	(evaluate deriv-result var val)

)

(defun evaluate (expr var val)

	(if (and (atom expr) (equal expr var))
		val
		(if (atom expr)
			expr	
			(cond
			   ((eq '+ (first expr)) ; PLUS
				    (splus (evaluate (second expr) var val) (evaluate (third expr) var val)))
  			   ((eq '* (first expr)) ; MULT
				    (smult  (evaluate (second expr) var val) (evaluate (third expr) var val)))
  			   ((eq '- (first expr)) 
				    (if(subsetp (butlast expr) (list '-) )
					(sunary (evaluate (second expr) var val)) ; UNARY
				    	(ssub  (evaluate (second expr) var val) (evaluate (third expr) var val)))) ;SUB
  			   ((eq '/ (first expr)) ; DIV
				    (sdiv (evaluate (second expr) var val) (evaluate (third expr) var val)))
  			   ((eq 'expt (first expr)) ; EXP
				    (sexpt (evaluate (second expr) var val) (evaluate (third expr) var val)))
  			   ((eq 'sqrt (first expr)) ; sqrt
				    (ssqrt (evaluate (second expr) var val)))
  			   ((eq 'log (first expr)) ; log
				    (slog (evaluate (second expr) var val)))
  			   ((eq 'exp (first expr)) ; power e
				    (sexp (evaluate (second expr) var val)))
  			   ((eq 'sin (first expr)) ; power e
				    (ssin (evaluate (second expr) var val)))
  			   ((eq 'cos (first expr)) ; power e
				    (scos (evaluate (second expr) var val)))
  			   ((eq 'tan (first expr)) ; power e
				    (stan (evaluate (second expr) var val)))
  			   (t ; Invalid
		       		  (error "Invalid Expression!"))
			)
		)
	)

)
