;; Propagation of constraints
;; Section 3.3.5

;; The Adder, Multiplier and Constant constraints

;; Adder

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else 
	   (error "Unknown request -- ADDER"))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;; Multiplier
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product 
		       (* (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? m1) (has-value? product))
	   (set-value! m2 
		       (/ (get-value product) (get-value m1))
		       me))
	  ((and (has-value? m2) (has-value? product))
	   (set-value! m1 
		       (/ (get-value product) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- MULTIPLIER"))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;; Constant

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT"))
  (connect connector me)
  (set-value! connector value me)
  me)

;; Probes
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else (error "Unknown request -- PROBE"))))
  (connect connector me)
  me)

;; syntax interfaces for using the "me" dispatch procedure of
;; constraints
(define (inform-about-value constraint)
  (constraint 'I-have-a-value)) 
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; Connectors
(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error make-connector "Contradiction"
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin
            (set! informant #f)
            (for-each-except retractor
                             inform-about-no-value
                             constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error make-connector "Unknown Operation"
                         request))))
    me))

;; The for-each-except procedure applies the designated procedure to
;; all item in the list except the given one
(define (for-each-except exception procedure lst)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop lst))

;; The syntax interface to the make-connector object
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;; The Celsius/Fahrenheit constraint network

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)))

#|
> (define cel (make-connector))
> (define farh (make-connector))
> (celsius-fahrenheit-converter cel farh)
#<procedure me>
> (probe "Celsius Temp" cel)
#<procedure me>
> (probe "Fahrenheit Temp" farh)
#<procedure me>
> (set-value! cel 25 'user)

Probe: Celsius Temp = 25
Probe: Fahrenheit Temp = 77done
> (set-value! farh 212 'user)
Unhandled exception
 Condition components:
   1. &error
   2. &who: #<procedure make-connector>
   3. &message: "Contradiction"
   4. &irritants: ((77 212))
> (forget-value! cel 'user)

Probe: Celsius Temp = ?
Probe: Fahrenheit Temp = ?done
> (set-value! farh 212 'user)

Probe: Fahrenheit Temp = 212
Probe: Celsius Temp = 100done
>
|#




