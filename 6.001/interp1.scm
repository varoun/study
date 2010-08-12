;;;; The scheme* evaluator

;;; Type checking

(define (tag-check e sym)
  (and (pair? e) (eq? (car e) sym)))

; sums
(define (sum? exp) (tag-check exp 'plus*))
; definitions
(define (define? exp) (tag-check exp 'define*))

;;; We need a table to store bindings. We use the hash table
;;; ADT that comes with scheme48

(define *environment* (make-table))

;;; The evaluator 

(define (eval* exp)
  (cond ((number? exp) exp)
	((sum? exp) (eval-sum exp))
	((symbol? exp) (lookup exp))
	((define? exp) (eval-define exp))
	(else (error "Unknown operation in eval*"))))


;;; Evaluating sums

(define (eval-sum exp)
  (+ (eval* (cadr exp)) (eval* (caddr exp))))

#|
> (eval* '(plus* 4 (plus* 5 6)))
15
>
|#

;;; looking up the value of a symbol in the enviroment
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
