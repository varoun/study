;;;; Exercise 2.33 - expressing basic list manipulation procedures as accumulations

;;; the accumulate procedure

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;; map as an accumulation
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

#|
> map
#{Procedure 8972 map}
> (define (square x) (* x x))
; no values returned
> (map square '(1 2 3 4 5))
'(1 4 9 16 25)
> 
|#

;;; append as accumulation
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

#|
> append
#{Procedure 8990 append}
> (append '(1 2 3 4) '(5 6 7 8))
'(1 2 3 4 5 6 7 8)
> 
|#

;;; length as accumulation
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
#|
> length
#{Procedure 8997 length}
> (length '(1 2 3 4 5))
5
> 
|#
