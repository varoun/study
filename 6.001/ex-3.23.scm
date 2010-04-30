;;;; Exercise 3.23 - Double Ended queues

;; We need a list-structure that has pointers to the previous
;; and the next pair for all operations to be O(1)

;;; Procedures used by the queue implementation

;; procedures for dealing with elements in queue
(define (make-queue-element item)
  (cons item (cons '() '()))) ; item + ptrs to the prev/next element

(define (queue-datum queue-item)
  (car queue-item))

(define (prev-ptr queue-item)
  (cadr queue-item))

(define (next-ptr queue-item)
  (cddr queue-item))

(define (set-prev-ptr! queue-item new-element)
  (set-car! (cdr queue-item) new-element))

(define (set-next-ptr! queue-item new-element)
  (set-cdr! (cdr queue-item) new-element))

;; procedures for dealing with the front and rear ptrs
(define (front-ptr dequeue)
  (car dequeue))

(define (rear-ptr dequeue)
  (cdr dequeue))

(define (set-front-ptr! dequeue element)
  (set-car! dequeue element))

(define (set-rear-ptr! dequeue element)
  (set-cdr! dequeue element))


;;; The actual constructors/selectors and mutators
;; A new deque is a pair of the front and rear ptrs
(define (make-dequeue)
  (cons '() '()))

(define (empty-dequeue? dequeue)
  (null? (front-ptr dequeue))) ; we can also check null? rear-ptr

(define (front-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "FRONT called with empty dequeue -- FRONT-DEQUEUE")
      (queue-datum (front-ptr dequeue))))

(define (rear-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "REAR called with empty dequeue -- REAR-DEQUEUE")
      (queue-datum (rear-ptr dequeue))))

(define (front-insert-dequeue! dequeue item)
  (let ((new-item (make-queue-element item)))
    (cond ((empty-dequeue? dequeue)
	   (set-front-ptr! dequeue new-item)
	   (set-rear-ptr! dequeue new-item)) ; dont return dq
	  (else                              ; contains cycle
	   (set-prev-ptr! (front-ptr dequeue) new-item)
	   (set-next-ptr! new-item (front-ptr dequeue))
	   (set-front-ptr! dequeue new-item)))))

(define (rear-insert-dequeue! dequeue item)
  (let ((new-item (make-queue-element item)))
    (cond ((empty-dequeue? dequeue)
	   (set-front-ptr! dequeue new-item)
	   (set-rear-ptr! dequeue new-item))
	  (else
	   (set-next-ptr! (rear-ptr dequeue) new-item)
	   (set-prev-ptr! new-item (rear-ptr dequeue))
	   (set-rear-ptr! dequeue new-item)))))

(define (front-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
	 (error "Empty dequeue -- FRONT-DELETE-DEQUEUE"))
	(else
	 (set-front-ptr! dequeue 
			 (next-ptr (front-ptr dequeue)))
	 (set-next-ptr! (prev-ptr (front-ptr dequeue)) '())
	 (set-prev-ptr! (front-ptr dequeue) '()))))

(define (rear-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
	 (error "EMPTY dequeue - REAR-DELETE-DEQUEUE"))
	(else
	 (set-rear-ptr! dequeue (prev-ptr (rear-ptr dequeue)))
	 (set-prev-ptr! (next-ptr (rear-ptr dequeue)) '())
	 (set-next-ptr! (rear-ptr dequeue) '()))))

;; printing a dequeue
(define (print-dequeue dequeue)
  (define (print-items dq-items)
    (if (null? (next-ptr dq-items))
	(begin
	  (display (queue-datum dq-items))
	  (newline))
	(begin
	  (display (queue-datum dq-items))
	  (print-items (next-ptr dq-items)))))
  (if (empty-dequeue? dequeue)
      (display "Dequeue empty")
      (print-items (front-ptr dequeue))))

#|
> (define dq (make-dequeue))
; no values returned
> (print-dequeue dq)
Dequeue empty#{Unspecific}
> (front-insert-dequeue! dq 'a)
#{Unspecific}
> (front-dequeue dq)
'a
> (rear-dequeue dq)
'a
> (front-insert-dequeue! dq 'b)
#{Unspecific}
> (front-dequeue dq)
'b
> (rear-dequeue dq)
'a
> (print-dequeue dq)
ba
#{Unspecific}
> (rear-insert-dequeue! dq 'c)
#{Unspecific}
> (print-dequeue dq)
bac
#{Unspecific}
> (rear-dequeue dq)
'c
> (rear-insert-dequeue! dq 'd)
#{Unspecific}
> (print-dequeue dq)
bacd
#{Unspecific}
> (front-delete-dequeue! dq)
#{Unspecific}
> (print-dequeue dq)
acd
#{Unspecific}
> (rear-delete-dequeue! dq)
#{Unspecific}
> (print-dequeue dq)
ac
#{Unspecific}
> 
|#

	       
	