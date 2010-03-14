;;;; Exercise 2.36 - accumulate-n

;;; accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;; accumulate-n
(define (accumulate-n op initial seqs)
  (if (null? (car seqs)) ; we need to look at the car cause at this point we have '(() () ())
      '()
      (cons (accumulate op initial (map car seqs))
	    (accumulate-n op initial (map cdr seqs)))))
#|
> (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
'(22 26 30)
> 
|#
;; this is similar to map applied to n lists
#|
> (map + '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))
'(22 26 30)
> 
|#
