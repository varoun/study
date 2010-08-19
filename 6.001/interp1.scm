;;;; The scheme* evaluator

;;; Type checking

(define (tag-check e sym)
  (and (pair? e) (eq? (car e) sym)))

; definitions
(define (define? exp) (tag-check exp 'define*))
; conditional - if?
(define (if? exp) (tag-check exp 'if*))
; applications
(define (application? exp) (pair? exp))

;;; We need a table to store bindings. We use the hash table
;;; ADT that comes with scheme48

(define *environment* (make-table))

;;; primitive: ADT that stores primitive operators in the env
(define prim-tag 'primitive)
(define (make-primitive scheme-proc) (list prim-tag scheme-proc))
(define (primitive? e) (tag-check e prim-tag))
(define (get-scheme-procedure prim) (cadr prim))

;;; initialise the environment
(table-set! *environment* 'plus* (make-primitive +))
(table-set! *environment* 'greater* (make-primitive >))
(table-set! *environment* 'true* #t)


;;; The evaluator 

(define (eval* exp)
  (cond ((number? exp) exp)
	((symbol? exp) (lookup exp))
	((define? exp) (eval-define exp))
	((if? exp) (eval-if exp))
	((application? exp) 
	 (apply* (eval* (car exp)) (map eval* (cdr exp))))
	(else 
	 (error "Unknown operation in eval*"))))

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

;;; Application
(define (apply* operator operands)
  (if (primitive? operator)
      (apply (get-scheme-procedure operator) operands)
      (error "Operator not a procedure: " operator)))

#|
> (eval* '(define* z* 9))
'unspecific
> (eval* '(plus* z* 6))
15
> (eval* '(if* true* 10 15))
10
|#
