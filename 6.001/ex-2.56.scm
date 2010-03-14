;;;; Exercise 2.56.scm - extending the symbolic deriv procedure to 
;;;; handle exponentiations

;;; deriv
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
	((exponentiation? exp)
	 (make-product 
	  (make-product (exponent exp) 
			(make-exponentiation (base exp) (make-difference (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else (error "unknown expression type in DERIV"))))

;;; the various constructors/selectors/predicates - 

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


;;; exponentiation
(define (exponentiation? e)
  (eq? (car e) '**))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((number=? exponent 0) 1)
	((number=? exponent 1) base)
	((and (number? base) (number? exponent))
	 (expt base exponent))
	(else (list '** base exponent))))

;;; difference
(define (make-difference a b)
  (cond ((number=? a 0) b)
	((number=? b 0) a)
	((and (number? a) (number? b)) (- a b))
	(else (list '- a b))))

#|
> (deriv '(+ (* a (** x 2)) (* b x) x) 'x)
'(+ (* a (* 2 x)) b)
> (deriv '(+ (* a (** x y)) (* b x) x) 'x)
'(+ (* a (* y (** x (- y 1)))) b)
>  
|#
