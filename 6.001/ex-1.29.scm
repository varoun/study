;;; Ex 1.29 - Integration by Simpson's rule

;; the higher-order sum procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term
	      (next a)
	      next
	      b))))

;; integration by simpsons rule, does not work, no easy way to get term
;; that includes the correct coefficient
#|(define (integral f a b n)
  (let ((h (/ n (- b a))))
    (* (/ h 3)
       (sum f
	    a
	    (lambda (x) (+ x h))
	    (+ a (* n h))))))
|#

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (val index) (+ a (* index h)))
  (define (inc x) (+ x 1))
  (define (current-term-value index)
    (cond ((or (= index 0) (= index 1)) (f (val index)))
	  ((even? index) (* 2 (f (val index))))
	  (else (* 4 (f (val index))))))
    (* (sum current-term-value a inc n)
     (/ h 3)))

	    
