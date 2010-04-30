;;;; Estimating the value of PI using Monte Carlo
;;;; simulation. State of the random numbner generator is
;;;; encapsulated in rand


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else 
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

#|
> ,open big-scheme
> ,load "/usr/local/share/scheme48-1.8/big/random.scm"

Warning: undefined variables
         #{Package 249 user}
         call-error
         (&warning)
> make-random
#{Procedure 8962 make-random}
> (define rand (make-random 99))
; no values returned
> (rand)
29988907
> (estimate-pi 100)
3.2163376045133845
> (estimate-pi 1000000)
3.137611024737104
> ,time (estimate-pi 10000000)
Run time: 107.26 seconds; Elapsed time: 107.53 seconds
3.137621063559069
>
|#


	   