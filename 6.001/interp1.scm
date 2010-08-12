;;;; The scheme* evaluator

;;; Type checking

(define (tag-check e sym)
  (and (pair? e) (eq? (car e) sym)))

; sums
(define (sum? exp) (tag-check exp 'plus*))


;;; The evaluator 

(define (eval* exp)
  (cond ((number? exp) exp)
	((sum? exp) (eval-sum exp))
	(else (error "Unknown operation in eval*"))))


;;; Evaluating sums

(define (eval-sum exp)
  (+ (eval* (cadr exp)) (eval* (caddr exp))))

#|
> (eval* '(plus* 4 (plus* 5 6)))
15
>
|#

