;;;; Constraint Propagation -- Chapter 23

;;; Classes for Assertions and Constraints

(defclass assertion ()
  ((name :accessor assertion-name :initarg :name)
   (lower-bound :accessor assertion-lower-bound :initform 0)
   (upper-bound :accessor assertion-upper-bound :initform 1)
   (constraints :accessor assertion-constraints :initform nil)))

(defclass constraint ()
  ((name :accessor constraint-name :initarg :name)
   (output :accessor constraint-output)))

(defclass binary-constraint (constraint)
  ((input :accessor constraint-input)))

(defclass ternary-constraint (constraint)
  ((input-a :accessor constraint-input-a)
   (input-b :accessor constraint-input-b)))

(defclass not-box (binary-constraint) ())

(defclass or-box (ternary-constraint) ())

(defclass and-box (ternary-constraint) ())

;;; Methods for wiring constraints and assertions together

(defmethod connect2 ((c constraint) (i assertion) (o assertion))
  "Connect a binary constraint to input and output assertions"
  (setf (constraint-input c) i)
  (setf (constraint-output c) o)
  (push c (assertion-constraints i))
  (push c (assertion-constraints o))
  'done)

(defmethod connect3 ((c constraint)
		     (a assertion)
		     (b assertion)
		     (o assertion))
  "Connect a ternary constraint to inputs and output"
  (setf (constraint-input-a c) a)
  (setf (constraint-input-b c) b)
  (setf (constraint-output c) o)
  (push c (assertion-constraints a))
  (push c (assertion-constraints b))
  (push c (assertion-constraints o))
  'done)

;;; The following procedure wires up the stock-split inference net 
;;; detailed in the chapter

(defun wire-stock-split-net ()
  "Wires the stock-split inference net"
  (let ((assertions
	 (list (make-instance 'assertion :name 'broker1)
	       (make-instance 'assertion :name 'broker2)
	       (make-instance 'assertion :name 'broker-opinion)
	       (make-instance 'assertion :name 'mystic1)
	       (make-instance 'assertion :name 'mystic2)
	       (make-instance 'assertion :name 'mystic-opinion)
	       (make-instance 'assertion :name 'your-opinion)))
	(constraints 
	 (list (make-instance 'or-box :name 'broker-constraint)
	       (make-instance 'or-box :name 'mystic-constraint)
	       (make-instance 'and-box :name 'your-constraint))))
    (dolist (l assertions)
      (set (assertion-name l) l)) ;assign assertion to symbol
    (dolist (c constraints)
      (set (constraint-name c) c)) ; assign constraint to symbol 
    (connect3 broker-constraint broker1 broker2 broker-opinion)
    (connect3 mystic-constraint mystic1 mystic2 mystic-opinion)
    (connect3 your-constraint broker-opinion mystic-opinion your-opinion)
    'done))

;;; The propagate-via-box generic function propagates values through 
;;; constraint boxes

(defmethod propagate-via-box ((constraint or-box))
  (let* ((a (constraint-input-a constraint))
	 (b (constraint-input-b constraint))
	 (o (constraint-output constraint))
	 (la (assertion-lower-bound a))
	 (ua (assertion-upper-bound a))
	 (lb (assertion-lower-bound b))
	 (ub (assertion-upper-bound b))
	 (lo (assertion-lower-bound o))
	 (uo (assertion-upper-bound o)))
    (propagate-via-assertion o constraint (max la lb) (+ ua ub))
    (propagate-via-assertion a constraint (- lo ub) uo)
    (propagate-via-assertion b constraint (- lo ua) uo)))

(defmethod propagate-via-box ((constraint and-box))
  (let* ((a (constraint-input-a constraint))
	 (b (constraint-input-b constraint))
	 (o (constraint-output constraint))
	 (la (assertion-lower-bound a))
	 (ua (assertion-upper-bound a))
	 (lb (assertion-lower-bound b))
	 (ub (assertion-upper-bound b))
	 (lo (assertion-lower-bound o))
	 (uo (assertion-upper-bound o)))
    (propagate-via-assertion o constraint (+ la lb -1) (min ua ub))
    (propagate-via-assertion a constraint 0 (+ 1 (- uo lb)))
    (propagate-via-assertion b constraint 0 (+ 1 (- uo la)))))

;;; The propagate-via-assertion updates the bounds and propagates the new
;;; bounds to connected constraints

(defmethod propagate-via-assertion ((assertion assertion)
				     (source constraint)
				     lower upper)
  (let* ((old-upper (assertion-upper-bound assertion))
	 (old-lower (assertion-lower-bound assertion))
	 (new-upper (max 0 (min old-upper upper)))
	 (new-lower (min 1 (max old-lower lower))))
    (unless (= old-upper new-upper)
      (setf (assertion-upper-bound assertion) new-upper))
    (unless (= old-lower new-lower)
      (setf (assertion-lower-bound assertion) new-lower))
    (when (or (/= old-lower new-lower)
	      (/= old-upper new-upper))
      (format t "~%Constraint ~a has modified ~a's values:" 
	      (constraint-name source)
	      (assertion-name assertion))
      (format t "~%[~4,2f, ~4,2f] ---> [~4,2f, ~4,2f]"
	      old-lower old-upper
	      new-lower new-upper)
      (dolist (constraint (assertion-constraints assertion))
	(propagate-via-box constraint)))))

;;; The initiate-propagation generic function initiates propagatation by
;;; supplying an assertion with values

(defmethod initiate-propagation ((assertion assertion) lower upper)
  (setf (assertion-lower-bound assertion) lower)
  (setf (assertion-upper-bound assertion) upper)
  (format t "~%You have started propagation from ~a with values:"
	  (assertion-name assertion))
  (format t "~%[~4,2f, ~4,2f]"
	  lower upper)
  (dolist (constraint (assertion-constraints assertion))
    (propagate-via-box constraint)))

#|
CL-USER> (initiate-propagation broker1 .25 .75)

You have started propagation from BROKER1 with values:
[0.25, 0.75]
Constraint BROKER-CONSTRAINT has modified BROKER-OPINION's values:
[0.00, 1.00] ---> [0.25, 1.00]
NIL
CL-USER> (initiate-propagation broker2 .33 .66)

You have started propagation from BROKER2 with values:
[0.33, 0.66]
Constraint BROKER-CONSTRAINT has modified BROKER-OPINION's values:
[0.25, 1.00] ---> [0.33, 1.00]
NIL
CL-USER> (initiate-propagation mystic1 .15 .15)

You have started propagation from MYSTIC1 with values:
[0.15, 0.15]
Constraint MYSTIC-CONSTRAINT has modified MYSTIC-OPINION's values:
[0.00, 1.00] ---> [0.15, 1.00]
NIL
CL-USER> (initiate-propagation mystic2 .85 .85)

You have started propagation from MYSTIC2 with values:
[0.85, 0.85]
Constraint MYSTIC-CONSTRAINT has modified MYSTIC-OPINION's values:
[0.15, 1.00] ---> [0.85, 1.00]
Constraint YOUR-CONSTRAINT has modified YOUR-OPINION's values:
[0.00, 1.00] ---> [0.18, 1.00]
NIL
CL-USER> 
|#

	  
      

    