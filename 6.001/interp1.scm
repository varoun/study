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
; lambda expressions
(define (lambda? exp) (tag-check exp 'lambda*))


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

(define (eval* exp env)
  (cond ((number? exp) exp)
	((symbol? exp) (lookup exp env))
	((define? exp) (eval-define exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (eval-lambda exp env))
	((application? exp) 
	 (apply* (eval* (car exp) env) 
		 (map (lambda (e) (eval* e env)) (cdr exp))))
	(else 
	 (error "Unknown operation in eval*"))))

;;; looking up the value of a symbol in the enviroment
;;; I should really use cond here, 'if' is not primitive!
(define (lookup name env)
  (if (null? env)
      (error "Unbound variable:" name)
      (let ((val (table-ref (car env) name)))
	(if val
	    val
	    (lookup name (cdr env))))))

;;; definitions -- adds a binding to the environment
(define (eval-define exp env)
  (let ((name (cadr exp))
	(defined-to-be (caddr exp)))
    (table-set! (car env) name (eval* defined-to-be env))
    'unspecific))


;;; Conditionals - evaluating if* expressions
;;; Our primitive conditional is the 'cond' (used in eval* for ex)
(define (eval-if exp env)
  (let* ((predicate (cadr exp))
	 (consequent (caddr exp))
	 (alternative (cadddr exp))
	 (test (eval* predicate env)))
    (cond 
     ((eq? test #t) (eval* consequent env))
     ((eq? test #f) (eval* alternative env))
     (else 
      (error "Predicate not a conditional: " predicate)))))

;;; Evaluating lambda expressions
;;; note: this actually does not eval anything.
;;; we just manipulate trees.
(define (eval-lambda exp env)
  (let ((args (cadr exp))
	(body (caddr exp))) ; we assume one expression in body
    (make-compound args body env)))


;;; Application
(define (apply* operator operands)
  (cond ((primitive? operator)
	 (apply (get-scheme-procedure operator) operands))
	((compound? operator)
	 (eval* (body operator)
		(extend-env-with-new-frame 
		 (parameters operator)
		 operands
		 (env operator))))
      (error "Operator not a procedure: " operator)))

;> (eval* '(define* z* 9) *environment*)
;'unspecific
;> (eval* '(if* (greater* z* 6)
;	       (plus* z* 2)
;	       15)
;	 *environment*)
;11
;> 

;;; ADT that implements the 'double-bubble'
(define compound-tag 'compound)
(define (make-compound parameters body env)
  (list compound-tag parameters body env))
(define (compound? exp) (tag-check exp compound-tag))
(define (parameters compound) (cadr compound))
(define (body compound) (caddr compound))
(define (env compound) (cadddr compound))

;;; The environment
(define (extend-env-with-new-frame names values env)
  (let ((new-frame (make-table))) ; make-table: hashtables in s48 
    (make-bindings! names values new-frame)
    (cons new-frame env)))

(define (make-bindings! names values frame)
  (for-each 
   (lambda (name value)
     (table-set! frame name value))
   names values))

;;; Initialise the global environment
(define *GE*
  (extend-env-with-new-frame
   (list 'plus* 'greater*)
   (list (make-primitive +) (make-primitive >))
   '()))


;> (eval* '(define* twice* (lambda* (x*) (plus* x* x*))) *GE*)
;'unspecific
;> (eval* '(twice* 4) *GE*)
;8
;> 
