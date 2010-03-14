;;;; Exercise 2.21 - squaring the elements of a list

;; square-list-1 - works by explicitly consing up a result list while cdring down the 
;; input list

(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list-1 (cdr items)))))

#|
> (square-list-1 (list 1 2 3 4 5))
'(1 4 9 16 25)
> 
|#

;; using map
(define (square-list-2 items)
  (map square items))

#|
> (square-list-2 (list 1 2 3 4 5))
'(1 4 9 16 25)
> 
|#



