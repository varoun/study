;;;; The scheme* evaluator

;;; Type checking

(define (tag-check e sym)
  (and (pair? e) (eq? (car e) sym)))

; sums
(define (sum? exp) (tag-check exp 'plus*))
; definitions
(define (define? exp) (tag-check exp 'define*))
; predicate - greater?
(define (greater? exp) (tag-check exp 'greater*))
; conditional - if?
(define (if? exp) (tag-check exp 'if*))

;;; We need a table to store bindings. We use the hash table
;;; ADT that comes with scheme48

(define *environment* (make-table))

;;; The evaluator 

(define (eval* exp)
  (cond ((number? exp) exp)
	((sum? exp) (eval-sum exp))
	((symbol? exp) (lookup exp))
	((define? exp) (eval-define exp))
	((greater? exp) (eval-greater exp))
	((if? exp) (eval-if exp))
	(else 
	 (error "Unknown operation in eval*"))))


;;; Evaluating sums

(define (eval-sum exp)
  (+ (eval* (cadr exp)) (eval* (caddr exp))))

#|
> (eval* '(plus* 4 (plus* 5 6)))
15
>
|#

;;; looking up the value of a symbol in the enviroment
;;; I should really use cond here, 'if' is not primitive!
(define (lookup name)
  (let ((val (table-ref *environment* name)))
    (if val
	val
	(error "Unbound variable" name))))

;;; definitions -- adds a binding to the environment
(define (eval-define exp)
  (let ((name (cadr exp))
	(defined-to-be (caddr exp)))
    (table-set! *environment* name (eval* defined-to-be))
    'unspecific))

#|
> (eval* '(define* x* (plus* 4 5)))
'unspecific
> (eval* '(plus* x* 2))
11
> 
|#

;;; Evaluating the greater* predicate
(define (eval-greater exp)
  (> (eval* (cadr exp)) (eval* (caddr exp))))

;;; Conditionals - evaluating if* expressions
;;; Our primitive conditional is the 'cond' (used in eval* for ex)
(define (eval-if exp)
  (let* ((predicate (cadr exp))
	 (consequent (caddr exp))
	 (alternative (cadddr exp))
	 (test (eval* predicate)))
    (cond 
     ((eq? test #t) (eval* consequent))
     ((eq? test #f) (eval* alternative))
     (else 
      (error "Predicate not a conditional: " predicate)))))

#|
> (eval* '(define* y* 9))
'unspecific
> (eval* '(if* (greater* y* 6) (plus* y* 2) 15))
11
> 
|#
