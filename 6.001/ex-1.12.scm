;;; Ex 1.12 - Pascals triangle by a recursive process

;;
(define (pascals row column)
  (cond ((= column 0) 1)
	((= row column) 1)
	(else 
	 (+ (pascals (- row 1) (- column 1))
	    (pascals (- row 1) column)))))

#| repl
> (pascals 0 0)
1
> (pascals 4 2)
6
> (pascals 1 1)
1
> (pascals 1 0)
1
> (pascals 2 1)
2
> 
#|

