;;;; Simulator for digital circuits
;;;; Section 3.3.4

;;; The logic gates are represented as procedures

;; The inverter
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 1) 0)
	((= s 0) 1)
	(else (error "Invalid signal -- LOGICAL-NOT"))))

;; The AND gate
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay 
		   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
	((or (= s1 0) (= s2 0)) 0)
	(else (error "Invalid signal -- LOGICAL-AND"))))

;; The OR GATE -- Ex-3.28
(define (or-gate i1 i2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal i1) (get-signal i2))))
      (after-delay or-gate-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! i1 or-action-procedure)
  (add-action! i2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
	((and (= s1 0) (= s2 0)) 0)
	(else (error "Invalid signal -- LOGICAL-OR"))))



;;; Wires which connect the digital function boxes are objects
;;; that are implemented using the message-passing style

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= new-value signal-value))
	  (begin
	    (set! signal-value new-value)
	    (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch msg . args) ; note the new defn - see icampus
      (cond ((eq? msg 'get-signal) signal-value)
	    ((eq? msg 'set-signal)
	     (set-my-signal! (car args)))
	    ((eq? msg 'add-action!)
	     (accept-action-procedure! (car args)))
	    (else (error "Unknown operation -- WIRE"))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  (wire 'set-signal! new-value))

(define (add-action! wire action-procedure)
  (wire 'add-action! action-procedure))

;; The probe
(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name)
		 (display "  ")
		 (display (current-time the-agenda))
		 (display "  New-Value = ")
		 (display (get-signal wire)))))

;;; The Agenda

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

;; The agenda is make of time segments, which is a pair of time
;; and a queue of procedures to be run at that time

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

;; The agenda itself is a one dimensional table (list), with 
;; the car holding the current time (time of last action)

(define (make-agenda) (list 0)) ; start time is 0

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue))) ; ,open queues
      (enqueue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= time (segment-time (car segments)))
	(enqueue! (segment-queue (car segments)) action)
	(let ((rest (cdr (segments))))
	  (if (belongs-before? rest)
	      (set-cdr! segments
			(cons (make-new-time-segment time action)
			      (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments! agenda
		       (cons (make-new-time-segment time action)
			     segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (dequeue! q)
    (if (queue-empty? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(car (queue->list (segment-queue first-seg))))))

;;; The half adder

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

#| The system is currently buggy - 
> (define the-agenda (make-agenda))
; no values returned
> (define (inverter-delay 2))

Warning: ill-formed definition
         (#{Name define} (inverter-delay 2))
         (&syntax-error)
'syntax-error
> (define inverter-delay 2)
; no values returned
> (define and-gate-delay 3)
; no values returned
> (define or-gate-delay 5)
; no values returned
> (define input-1 (make-wire))
; no values returned
> (define input-2 (make-wire))
; no values returned
> (define sum (make-wire))
; no values returned
> (define carry (make-wire))
; no values returned
> (probe 'sum sum)

sum  0  New-Value = 0#{Unspecific}
> (probe 'carry carry)

carry  0  New-Value = 0#{Unspecific}
> 
> (half-adder input-1 input-2 sum carry)

Error: attempt to call a non-procedure
       ('((2 . #{Queue}) (3 . #{Queue}) (5 . #{Queue})))
1> ,reset

Top level
> ,step half-adder
Unrecognized command name.
>
> 
> (or-gate input-1 input-2)

Error: wrong number of arguments
       ('#{Procedure 8550 or-gate} '#{Procedure 8561 (dispatch in make-wire)} '#{Procedure 8561 (dispatch in make-wire)})
1> ,reset

Top level
> (and-gate input-1 input-2 sum)

Error: attempt to call a non-procedure
       ('((2 . #{Queue}) (3 . #{Queue}) (5 . #{Queue})))
1> 
|#
