;;; Exercise 1.37 - k-term finite continued fraction

;; recursive process

(define (cont-frac n d k)
  (define (recur count)
    (if (= count k)
	(/ (n count) (d count))
	(/ (n count) (+ (d count) (recur (+ count 1))))))
  (recur 0))

#|
> (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     100)
0.6180339887498948 ; this is the val of 1/golden-ratio
> (/ 1 0.6180339887498948)
1.618033988749895 ; golden ratio
> 
|#

;; iterative process
(define (cont-frac-i n d k)
  (define (iter result count)
    (if (= count 0)
	(/ (n count) (+ (d count) result))
	(iter (/ (n count) (+ (d count) result)) (- count 1))))
  (iter (/ (n k) (d k)) k))

#|
> (cont-frac-i (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     100)
0.6180339887498948
> |#

;; Exercise 1.38 - approximating e using cont-frac
;; There is a more efficient way of doing this where the value is a 
;; function of index- check the web!
(define (f-fun index) ; generate seq 1 2 1 1 4 1 1 6 1 1 8
  (define (iter d-2 d-1 d-cur count)
    (cond ((= count index) d-cur)
	  ((and (= d-1 1) (= d-cur 1))
	   (iter d-1 d-cur (+ d-1 d-2 d-cur) (+ count 1)))
	  (else (iter d-1 d-cur 1 (+ count 1)))))
  (cond ((= index 0) 1)
	((= index 1) 2)
	((= index 2) 1)
	(else (iter 1 2 1 2))))

;; the value of e
(define (e-val k)
  (+ 
   (cont-frac (lambda (i) 1.0)
	      f-fun
	      k)
   2))

#|
> (e-val 10)
2.7182818352059925
> 
|#

