;;;; Section 3.1.1 - Local state

;; make-withdraw
(define (make-withdraw balance)
  (lambda (amount)
    (if (<= amount balance)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient Funds")))
#|
> (define a1 (make-withdraw 100))
> (a1 10)
90
> (a1 10)
80
> (a1 90)
"Insufficient Funds"
> 
|#

;; make-account
(define (make-account balance)
  (define (withdraw amount)
    (if (<= amount balance)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"))))
  dispatch)
#|
> (define acc (make-account 100))
; no values returned
> acc
#{Procedure 8557 (dispatch in make-account)}
> ((acc 'withdraw) 25)
75
> ((acc 'deposit) 10)
85
> ((acc 'meh) 1000)

Error: Unknown request -- MAKE-ACCOUNT
       (&error)
1>
|#

