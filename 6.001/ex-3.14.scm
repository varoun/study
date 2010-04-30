;;;; Exercise 3.14 - 

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

;; mystery destructively reverses the list

#|
> (define v (list 'a 'b 'c 'd))
; no values returned
> v
'(a b c d)
> (define w (mystery v))
; no values returned
> w
'(d c b a)
> v
'(a)
> 
|#

	
	