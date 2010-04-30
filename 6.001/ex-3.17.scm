;;;; Exercise 3.17 - 
;;;; Count the correct number of pairs in list-structure

(define (count-pairs x)
  (define iter
    (let ((seen '()))
      (lambda (pairs)
	(cond ((not (pair? pairs)) 0)
	      ((memq pairs seen) 0)
	      (else 
	       (set! seen (cons pairs seen))
	       (+ 1 (iter (car pairs)) (iter (cdr pairs))))))))
  (iter x))

#|
> (define s1 (list 'a))
; no values returned
> (define s2 (list 'b))
; no values returned
> (define p1 (cons s1 s2))
; no values returned
> p1
'((a) b)
> 
> (count-pairs p1)
3
> (set-cdr! s1 s2)
#{Unspecific}
> p1
'((a b) b)
> (count-pairs p1)
3
> (set-cdr! p1 s1)
#{Unspecific}
> p1
'((a b) a b)
> (set-car! s1 s2)
#{Unspecific}
> p1
'(((b) b) (b) b)
> (count-pairs p1)
3
> 
|#

;; We seen a seperate iter procedure because we do not want the 
;; binding for 'seen' to persist between calls to count-pairs

       
     
	      
	