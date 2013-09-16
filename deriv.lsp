;derivative rule for addition
(defun derivplus (expr var)
	(splus (deriv (second expr) var)  (deriv (third expr) var))
)

;derivative rule for multiplication
(defun derivmult (expr var)
	(splus  (smult (third expr) (deriv (second expr) var))  (smult (second expr) (deriv (third expr) var)))
)

;derivative rule for division
(defun derivdiv (expr var)

	(sdiv (ssub (smult (third expr) (deriv (second expr) var)) (smult (second expr) (deriv (third expr) var))) (smult (third expr) (third expr)))
)

;derivative rule for subtraction
(defun derivsub (expr var)

	(ssub (deriv (second expr) var)  (deriv (third expr) var))
)

;derivative rule for unary minus
(defun derivunary (expr var)

	(sunary (deriv (second expr) var))
)

;derivative rule for exponent
(defun derivexpt (expr var)
	
	(smult (third expr) (smult (sexpt (second expr) (ssub (third expr) 1)) (deriv (second expr) var)))
)

;derivative rule for square root
(defun derivsqrt (expr var)
	
	(smult (sdiv 1 2) (smult (sexpt (second expr) (ssub (sdiv 1 2) 1)) (deriv (second expr) var)))
)

;derivative rule for logarithms
(defun derivlog (expr var)
	
	(smult (sdiv 1 (second expr)) (deriv (second expr) var))
)

;derivative rule for power e
(defun derivexp (expr var)
	
	(smult (sexp (second expr)) (deriv (second expr) var))
)

;derivative rule for sine
(defun derivsin (expr var)
	
	(smult (scos (second expr)) (deriv (second expr) var))
)

;derivative rule for cosine
(defun derivcos (expr var)
	
	(sunary (smult (ssin (second expr)) (deriv (second expr) var)))
)

;derivative rule for tangent
(defun derivtan (expr var)
	
	(smult (splus 1 (sexpt (stan (second expr)) 2)) (deriv (second expr) var))
)

;Function to find the derivative for the expression <expr> with respect to variable <var>
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
					(derivunary expr var) ;If number of arguments = 1, UNARY
				    	(derivsub expr var))) ;If number of arguments > 1, SUB
  			   ((eq '/ (first expr)) ; DIV
				    (derivdiv expr var))
  			   ((eq 'expt (first expr)) ; EXPONENT
				    (derivexpt expr var))
  			   ((eq 'sqrt (first expr)) ; sqrt
				    (derivsqrt expr var))
  			   ((eq 'log (first expr)) ; log
				    (derivlog expr var))
  			   ((eq 'exp (first expr)) ; power e
				    (derivexp expr var))
  			   ((eq 'sin (first expr)) ; SIN
				    (derivsin expr var))
  			   ((eq 'cos (first expr)) ; COS
				    (derivcos expr var))
  			   ((eq 'tan (first expr)) ; TAN
				    (derivtan expr var))
  			   (t ; Invalid
		       		  (error "Invalid Expression!"))
 		    )
	 )
)

;Simplification function  that can recursively simplify expression <expr>
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
					(sunary (simplify (second expr))) ;If number of arguments = 1, UNARY
				    	(ssub  (simplify (second expr)) (simplify (third expr))))) ;If number of arguments >1, SUB
  			   ((eq '/ (first expr)) ; DIV
				    (sdiv (simplify (second expr)) (simplify (third expr))))
  			   ((eq 'expt (first expr)) ; EXPONENT
				    (sexpt (simplify (second expr)) (simplify (third expr))))
  			   ((eq 'sqrt (first expr)) ; sqrt
				    (ssqrt (simplify (second expr))))
  			   ((eq 'log (first expr)) ; log
				    (slog (simplify (second expr))))
  			   ((eq 'exp (first expr)) ; power e
				    (sexp (simplify (second expr))))
  			   ((eq 'sin (first expr)) ; SIN
				    (ssin (simplify (second expr))))
  			   ((eq 'cos (first expr)) ; COS
				    (scos (simplify (second expr))))
  			   ((eq 'tan (first expr)) ; TAN
				    (stan (simplify (second expr))))
  			   (t ; Invalid
		       		  (error "Invalid Expression!"))
		)
	)
)



;Simplification function for Addition
(defun splus (x y)
	(if (numberp x)
		(if (numberp y)
			(+ x y)   ; return the added val if both args are numbers
			(if (zerop x)
				y ; return the other operand if one operand is zero
				(list '+ x y)
			)
		)
		(if (and (numberp y) (zerop y))
			x          ;return the other operand if one operand is zero
			(list '+ x y)
		)
	)
)

;Simplification function for Unary minus
(defun sunary (x)
	(if (numberp x)
		(* -1 x)   ;return -1* value if arg is a number
		(list '- x)
	)
)

;Simplification function for Subtraction
(defun ssub (x y)
	(if (numberp x)
		(if (numberp y)
			(- x y) ;return the sub val if both args are numbers
			(if (zerop x)
				(sunary y) ; return unary minus if first number if zero
				(list '- x y)
			)
		)
		(if (and (numberp y) (zerop y))
			x        ; return the first arg if second arg is zero
			(if(equal x y)
				0   ; return zero if both args are same
				(list '- x y)
			)
		)
	)
)

;Simplification function for multiplication
(defun smult (x y)
	(if (numberp x)
		(if (numberp y)
			(* x y)          ;return product value if both args are numbers
			(if (zerop x)
				0	 ;return 0 if one of the args is zero
				(if (eq x 1)
				  y      ;return the other operand if one operand is 1
				  (if(eq x -1)
					(sunary y) ; return minus unary val of one operand if other is -1
				  	(list '* x y)
				   )
				)
			)
		)
		(if(numberp y)
			(if(zerop y)
				0	;return 0 if one of the args is zero
				(if (eq y 1)
					x    ;return the other operand if one operand is 1
					(if(eq y -1)
						(sunary x)  ;return minus unary val of one operand if other is -1
						(list '* x y)
					)
				)		
			)	
			(list '* x y)
		)
	)
)

;Simplification function for division
(defun sdiv (x y)
	(if (eq 0 y)                               
            (print " Division by Zero. Error !!")   ; return error if denominator is zero
	    (if (eq 0 x)
		0				    ; return 0 if numerator is zero
		(if (equal x y)
			1			    ; return 1 if numerator is equal to denominator
			(if (eq y 1)
				x		    ; return numerator if denominator is 1
				(if (eq y -1)
					(sunary x)  ; return minus unary value of numerator if denominator is -1
					(if (and (numberp x) (numberp y))
						(/ x y)	; return div value if both args are numbers
				 		(list '/ x y)
					)
				)
			)
		)
	     )
	)
)

;Simplification function for exponent
(defun sexpt (x y)
	(if (and (numberp x) (numberp y))
		(expt x y)         ; return exponent value if both arguments are numbers
		(if (and  (numberp y) (eq y 0))
			1          ; return 1 if exponent is 0
			(if (and (numberp y)(eq y 1))
				x  ;return the base if exponent is 1
				(list 'expt x y)
			)
		)
	)
)				

;Simplification function for Power e			
(defun sexp (x)
	(if(numberp x)
		(exp x)          ; return power value to e if arg is a number
		(list 'exp x)
	  )
)	

;Simplification function for log
(defun slog (x)
	(if(and (numberp x) (zerop x))
		(print "Log error as argument is zero" ) ;throw error if arg is zero
		(if (numberp x)
			(log x)    ; return LOG if arg is a number
			(list 'log x)
		)
	  )
)

;Simplification function for Square root
(defun ssqrt (x)
	(if(and (numberp x) (minusp x))
		(print "sqrt error as argument is negative" )  ; throw error if arg is a negative number
		(if (numberp x)
			(sqrt x)     ; return square root if arg is a number         
			(list 'sqrt x)
		)
	  )
)

;Simplification function for SIN
(defun ssin (x)	
	(if (numberp x)	
		(sin x)   ; return SIN value if arg is a number
		(list 'sin x)
	)
)

;Simplification function for COS
(defun scos (x)	
	(if (numberp x)
		(cos x)    ;return COS value if arg is a number
		(list 'cos x)
	)
)

;Simplification function for TAN
(defun stan (x)	
	(if (numberp x)
		(tan x)     ; return TAN value if arg is a number
		(list 'tan x)
	)
)

;Implementation for Deriv-eval.Assign numerical value <val> to derivative of expression <expr> wrt to variable <var>
(defun deriv-eval (expr var val)

	(setq deriv-result (deriv expr var))
	(evaluate deriv-result var val)

)
;Recursive implementation of evaluation function when <var> has to be replaced with <val> in the expression <expr>
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
					(sunary (evaluate (second expr) var val)) ; If number of arguments = 1, UNARY
				    	(ssub  (evaluate (second expr) var val) (evaluate (third expr) var val)))) ;If number of arg >1,SUB
  			   ((eq '/ (first expr)) ; DIV
				    (sdiv (evaluate (second expr) var val) (evaluate (third expr) var val)))
  			   ((eq 'expt (first expr)) ; EXPONENT
				    (sexpt (evaluate (second expr) var val) (evaluate (third expr) var val)))
  			   ((eq 'sqrt (first expr)) ; sqrt
				    (ssqrt (evaluate (second expr) var val)))
  			   ((eq 'log (first expr)) ; log
				    (slog (evaluate (second expr) var val)))
  			   ((eq 'exp (first expr)) ; power e
				    (sexp (evaluate (second expr) var val)))
  			   ((eq 'sin (first expr)) ; SIN
				    (ssin (evaluate (second expr) var val)))
  			   ((eq 'cos (first expr)) ; COS
				    (scos (evaluate (second expr) var val)))
  			   ((eq 'tan (first expr)) ; TAN
				    (stan (evaluate (second expr) var val)))
  			   (t ; Invalid
		       		  (error "Invalid Expression!"))
			)
		)
	)

)
