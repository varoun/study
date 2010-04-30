;;;; Queues represented as closures (procedures with state)

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (queue-empty?)
      (null? front-ptr))
    (define (front-queue)
      (if (queue-empty?)
	  (error "FRONT called with empty queue -- FRONT-QUEUE")
	  (car front-ptr)))
    (define (print-queue)
      (newline)
      (display front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (list item)))
	(cond ((queue-empty?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (set! front-ptr (cdr front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) queue-empty?)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'print-queue) print-queue)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    (else 
	     (error "Unknown operation -- QUEUE"))))
    dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (front-queue queue)
  ((queue 'front-queue)))

(define (print-queue queue)
  ((queue 'print-queue)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

#|
> (define q1 (make-queue))
; no values returned
> (insert-queue! q1 'a)
#{Unspecific}
> (insert-queue! q1 'b)
#{Unspecific}
> (print-queue q1)

(a b)#{Unspecific}
> (delete-queue! q1)
#{Unspecific}
> (front-queue q1)
'b
> (print-queue q1)

(b)#{Unspecific}
> 
|#

;; the mutators dont return any values - what should they return
;; should they return the dispatch procedure ?
