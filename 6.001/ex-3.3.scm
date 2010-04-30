;;;; Exercise 3.3 / 3.4 - Password protected accounts

(define (make-account balance password)
  (define (withdraw amount)
    (if (<= amount balance)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((tries 0))
    (lambda (pass m)
      (cond ((and (> tries 6) (not (eq? pass password)))
	     (lambda (x) "Calling Cops"))
	    ((not (eq? pass password))
	     (lambda (x) "Incorrect Password"))
	    ((eq? m 'withdraw) 
	     (set! tries 0)
	     withdraw)
	    ((eq? m 'deposit) 
	     (set! tries 0)
	     deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT"))))))

#|

> (define acc (make-account 100 'foobar))
; no values returned
> ((acc 'foobar 'withdraw) 25)
75
> ((acc 'foobar 'deposit) 10)
85
> ((acc 'wrongpass 'withdraw) 20)
"Incorrect Password"
> ((acc 'wrongpass2 'withdraw) 20)
"Incorrect Password"
> 

|#

;; Note that this version always sets "tries" to zero even if it is 
;; currently zero. A more efficient version should probably check
;; the current value of "tries" and then reset it only if needed ?
