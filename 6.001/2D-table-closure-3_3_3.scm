;;;; 2D table implemented as closures

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record 
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value) 
				  (cdr subtable)))))
	    (set-cdr! local-table 
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table))))))
    (define (dispatch msg)
      (cond ((eq? msg 'lookup-proc) lookup)
	    ((eq? msg 'insert-proc) insert!)
	    (else
	     (error "Unknown operation - MAKE-TABLE"))))
    dispatch))

#|
> (define operation-table (make-table))
; no values returned
> (define put (operation-table 'insert-proc))
; no values returned
> (define get (operation-table 'lookup-proc))
; no values returned
> get
#{Procedure 8529 (lookup in make-table)}
> put
#{Procedure 8530 (insert! in make-table)}
> (put 'a 1 'a1)
#{Unspecific}
> (put 'a 2 'a2)
#{Unspecific}
> (put 'b 1 'b1)
#{Unspecific}
> (put 'b 2 'b2)
#{Unspecific}
> (put 'c 1 'c1)
#{Unspecific}
> (put 'c 2 'c2)
#{Unspecific}
> (get 'a 2)
'a2
> (get 'c 1)
'c1
> 
|#

				  