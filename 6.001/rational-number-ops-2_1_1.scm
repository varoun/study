;;; Rational numbers

;; constructors and selectors - these are the interface procedures

(define (make-rat n d) 
  (cons n d))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

;; Operations on rational numbers

; printing a rational number
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
	   
	 
; Addition
(define (add-rat x y)
  (make-rat 
   (+ (* (numer x) (denom y)) 
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

; Multiplication/division etc can be implemented in a similar fashion

#| repl
> (define r1 (make-rat 1 3))
; no values returned
> (print-rat r1)

1/3#{Unspecific}
> (define r2 (make-rat 1 3))
; no values returned
> (print-rat (add-rat r1 r2))

6/9#{Unspecific} ; a better version of rational numbers will represent these in lowest terms
> 
|#
