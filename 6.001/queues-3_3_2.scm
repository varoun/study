;;;; Queue implementation

;;; The following procedures are used by the implementation

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item)
  (set-car! q item))
(define (set-rear-ptr! q item)
  (set-cdr! q item))

;;; Constructors/selectors/mutators for queues

;; constructor
(define (make-queue)
  (cons '() '()))

;; selectors
(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "Front called on an empty queue -- FRONT-QUEUE")
      (car (front-ptr q))))

;; mutators
(define (insert-queue! q item)
  (let ((new-pair (list item)))
    (cond ((empty-queue? q)
	   (set-front-ptr! q new-pair)
	   (set-rear-ptr! q new-pair)
	   q)
	  (else
	   (set-cdr! (rear-ptr q) new-pair)
	   (set-rear-ptr! q new-pair)
	   q))))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "DELETE called with empty queue -- DELETE-QUEUE")
      (begin
	(set-front-ptr! q (cdr (front-ptr q)))
	q)))

#|
> (define q1 (make-queue))
; no values returned
> q1
'(())
> (insert-queue! q1 'a)
'((a) a)
> (insert-queue! q1 'b)
'((a b) b)
> (delete-queue! q1)
'((b) b)
> (delete-queue! q1)
'(() b)
> (empty-queue? q1)
#t
> 
|#

;;; Exercise 3.21

;; Procedure to correctly print queues
(define (print-queue q)
  (display (front-ptr q)))

#|
> (print-queue q1)
()#{Unspecific}
> (insert-queue! q1 'a)
'((a) a)
> (insert-queue! q1 'b)
'((a b) b)
> (print-queue q1)
(a b)#{Unspecific}
> 
|#
