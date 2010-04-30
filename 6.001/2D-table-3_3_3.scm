;;;; Two dimensional tables 

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable 
			(cons (cons key-2 value) (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1 (cons key-2 value))
			(cdr table))))))


(define (make-table)
  (list '*table*))

#|
> (define t1 (make-table))
; no values returned
> (insert! 'a '1 'a1 t1)
#{Unspecific}
> (insert! 'a '2 'a2 t1)
#{Unspecific}
> t1
'(*table* (a (2 . a2) (1 . a1)))
> (insert! 'b 2 'b2 t1)
#{Unspecific}
> (insert! 'b 1 'b1 t1)
#{Unspecific}
> t1
'(*table* (b (1 . b1) (2 . b2)) (a (2 . a2) (1 . a1)))
> (insert! 'c 1 'c1 t1)
#{Unspecific}
> (insert! 'c 2 'c2 t1)
#{Unspecific}
> t1
'(*table* (c (2 . c2) (1 . c1)) (b (1 . b1) (2 . b2)) (a (2 . a2) (1 . a1)))
> (lookup 'b 2 t1)
'b2
> 
#|


			