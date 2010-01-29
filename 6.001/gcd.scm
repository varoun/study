;; 1.2.5 - GCD using Euclid's Algo

(define (gcd-euclid a b)
  (if (= b 0)
      a
      (gcd-euclid b (remainder a b))))

;; repl
;> (gcd-euclid 40 6)
;2
;> 