;;; Exercise 2.2 - Line segments

;; start by constructors and selectors for representing points

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

; printing a point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
;; a line segment is a pair of points - start and end

(define (make-segment start end)
  (cons start end))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))


;; the mid-point of a line segment

(define (mid-point line)
  (define (average n1 n2) (/ (+ n1 n2) 2))
  (let ((x1 (x-point (start-segment line)))
	(y1 (y-point (start-segment line)))
	(x2 (x-point (end-segment line)))
	(y2 (y-point (end-segment line))))
    (make-point (average x1 x2)
		(average y1 y2))))

#| repl
> (define p1 (make-point 2 4))
; no values returned
> (print-point p1)

(2,4)
#{Unspecific}
> (define p2 (make-point 6 8))
; no values returned
> (define l1 (make-segment p1 p2))
; no values returned
> (print-point (mid-point l1))

(4,6)
#{Unspecific}
> 
|#
