;;;; Symbolic differentiation 

;;; The "big-scheme" package is needed for the error procedure

;;; define the deriv procedure first based on abstract data specified in terms of
;;; the various constructors and selectors - wishfull thinking
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp) (deriv (multiplicand exp) var))
	  (make-product (multiplicand exp) (deriv (multiplier exp) var))))
	(else (error "unknown expression type in DERIV"))))

#| Evaluating the above results in - 
> 
Warning: undefined variables
         #{Package 249 user}
         variable?
         same-variable?
         sum?
         addend
         augend
         make-sum
         product?
         multiplier
         multiplicand
         make-product
         (&warning)

> 
|#

;;; define the constructors/selectors/predicates for the abstract data
;;; initial versions of sums/products did not reduce to simpler terms -
#|
> (deriv '(+ x 3) 'x)
'(+ 1 0)
> (deriv '(* x y) 'y)
'(+ (* x 1) (* y 0))
> 
|#

;;; the number=? procedure needed by make-sum/make-product to simplify terms
(define (number=? exp num)
  (and (number? exp) (= exp num)))

;;; variables
(define (variable? e) (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;; sums
(define (sum? e)
  (eq? (car e) '+))

(define (addend e) (cadr e))

(define (augend e) (caddr e))

(define (make-sum a1 a2)
  (cond ((number=? a1 0) a2)
	((number=? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

;;; products
(define (product? e)
  (eq? (car e) '*))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-product m1 m2)
  (cond ((number=? m1 1) m2)
	((number=? m2 1) m1)
	((or (number=? m1 0) (number=? m2 0)) 0)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

#|
> (deriv '(+ x 3) 'x)
1
> (deriv '(* x y) 'y)
'x
> (deriv '(* (* x y) (+ x 3)) 'x)
'(+ (* x y) (* (+ x 3) y))
> 
|#

  
  

  