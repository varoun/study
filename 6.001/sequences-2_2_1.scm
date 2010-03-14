;;;; Representing sequences - Section 2.2.1

;;; list-ref
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
#|
> (list-ref '(1 2 3 4 5) 2)
3
> (list-ref '(1 2 3 4 5) 7)

Error: vm-exception
       (cdr '())
1>
|#

;;; list-length
(define (list-length items)
  (if (null? items)
      0
      (+ 1 (list-length (cdr items)))))

(define (list-length-iter items)
  (define (iter items count)
    (if (null? items)
	count
	(iter (cdr items) (+ count 1))))
  (iter items 0))

#|
> (list-length (list 1 2 3 4 5))
5
> 
> (list-length-iter (list 1 2 3 4 5 6))
6
> 
|#

;;; list-append
(define (list-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (list-append (cdr list1) list2))))
#|
> (list-append (list 1 2 3 4 5) (list 6 7 8 9 10))
'(1 2 3 4 5 6 7 8 9 10)
> 
|#
