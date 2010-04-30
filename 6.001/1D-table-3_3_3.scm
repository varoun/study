;;;; One dimensional table implementation

;; The lookup procedure is defined in terms of the assoc
;; procedure that is a primitive in s48

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record 
	(cdr record)
	#f)))

;; Inserting a key/val pair into a table
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table 
		  (cons (cons key value) (cdr table))))))

;; the table constructor
(define (make-table)
  (list '*table*))

#|
> (define t1 (make-table))
; no values returned
> t1
'(*table*)
> (insert! 'a 1 t1)
#{Unspecific}
> t1
'(*table* (a . 1))
> (insert! 'b 2 t1)
#{Unspecific}
> (insert 'c 3 t1)
#{Unspecific}
> (lookup 'a t1)
1
> (lookup 'c t1)
3
> (insert! 'a 10 t1)
#{Unspecific}
> (lookup 'a t1)
10
> 
> (lookup 'd t1)
#f
> 
#|
